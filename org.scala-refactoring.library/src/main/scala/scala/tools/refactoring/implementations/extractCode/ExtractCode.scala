package scala.tools.refactoring
package implementations.extractCode

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.Flags
import scala.tools.refactoring.MultiStageRefactoring

abstract class ExtractCode extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

  /**
   * Represents a target scope for the extracted code and all optional
   * and required parameters if the extracted expression would be inserted
   * in this scope.
   */
  case class TargetScope(
    /** tree representing the scope */
    tree: Tree,
    /** optional parameters for a new method in this scope */
    optionalParameters: List[Symbol],
    /** required parameters for a new method in this scope*/
    requiredParameters: List[Symbol])

  case class PreparationResult(
    /** tree to insert the new closure definition */
    enclosingTree: Tree,
    /** optional parameters of the closure */
    optionalParameters: List[Symbol],
    /** required parameters of the closure */
    requiredParameters: List[Symbol])

  case class RefactoringParameters(
    /** name of the closure function */
    closureName: String,
    /** returns for each optional parameter if it should be used as a closure parameter */
    closureParameters: Symbol => Boolean)

  def prepare(selection: Selection): Either[PreparationError, PreparationResult] = {

    def mkPreparationResult(enclosingTree: Tree) = {
      val childContainingSelection = enclosingTree.filter { t =>
        t != enclosingTree &&
          t.pos.isRange && t.pos.start < selection.pos.start &&
          t.exists(p => p == selection.allSelectedTrees.head)
      }.headOption.getOrElse(EmptyTree)

      val deps = inboundDependencies(selection)
        .filter(s => s.isValue && !s.isConstructor)

      val newSymbolsBetweenEnclosingAndSelection = childContainingSelection.filter {
        case t: DefTree => !selection.contains(t)
        case _ => false
      }.map(_.symbol)

      val (requiredParams, optionalParams) = deps.partition { sym =>
        newSymbolsBetweenEnclosingAndSelection.contains(sym)
      }

      PreparationResult(enclosingTree, optionalParams, requiredParams)
    }

    val definesNonLocal = selection.selectedSymbols.exists { sym =>
      !sym.isLocal && index.declaration(sym).exists(selection.contains(_))
    }

    val definesNonValue = selection.selectedTopLevelTrees.exists {
      case t: MemberDef => !t.isInstanceOf[ValDef]
      case _ => false
    }

    if (definesNonLocal)
      Left(PreparationError("Can't extract expression that defines non local fields."))
    else if (definesNonValue)
      Left(PreparationError("Can't extract expression that defines non-value symbols."))
    else if (selection.selectedTopLevelTrees.size == 0)
      Left(PreparationError("No expression or statement selected."))
    else
      selection.findSelectedWithPredicate { // find a tree to insert the closure definition
        case b: Block if b == selection.selectedTopLevelTrees.head => false
        case _: DefDef | _: Block | _: Template => true
        case _ => false
      } match {
        case Some(t) => Right(mkPreparationResult(t))
        case _ => Left(PreparationError("Can't extract closure from this position."))
      }
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val selectedOptionalParams = preparation.optionalParameters.filter(userInput.closureParameters(_))
    val params = inboundDependencies(selection)
      .filter { dep =>
        preparation.requiredParameters.contains(dep) ||
          selectedOptionalParams.contains(dep)
      }
    val returns = outboundLocalDependencies(selection).distinct

    val closure = {
      val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
      val closureBody = selection.selectedTopLevelTrees match {
        // a single block tree could be unpacked
        case Block(stats, expr) :: Nil => stats ::: expr :: returnStatement
        case stats => stats ::: returnStatement
      }
      val mods = preparation.enclosingTree match {
        case _: Template => NoMods withPosition (Flags.PRIVATE, NoPosition)
        case _ => NoMods
      }

      mkDefDef(
        mods,
        userInput.closureName,
        if (params.isEmpty) Nil else params :: Nil,
        closureBody)
    }

    val call = mkCallDefDef(userInput.closureName, params :: Nil, returns)

    val extractSingleStatement = selection.selectedTopLevelTrees.size == 1

    val replaceExpressionWithCall =
      replaceTree(selection.selectedTopLevelTrees.head, call)

    val replaceSequenceWithCall =
      transform {
        case block @ Block(stats, expr) => {
          val newStats = (stats :+ expr).replaceSequence(selection.selectedTopLevelTrees, call :: Nil)
          mkBlock(newStats) replaces block
        }
      }

    val findEnclosingTree = predicate { (t: Tree) => t == preparation.enclosingTree }

    def insertClosureInSequence(statements: List[Tree]) = {
      val (before, after) = statements.span { t =>
        t.pos.isRange && t.pos.end <= selection.pos.start
      }
      if (before.length == 0)
        closure :: after
      else
        before ::: PlainText.BlankLine :: closure :: after
    }

    val insertClosureDef = transform {
      case t @ Block(stats, expr) =>
        t copy (stats = insertClosureInSequence(stats)) replaces t
      case t @ Template(_, _, body) =>
        t copy (body = insertClosureInSequence(body)) replaces t
      case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
        t copy (rhs = Block(closure :: Nil, rhs)) replaces t
    }

    val extractClosure = topdown {
      matchingChildren {
        findEnclosingTree &>
          {
            // first try to replace direct children
            (if (extractSingleStatement) replaceExpressionWithCall else replaceSequenceWithCall) |>
              // otherwise replace in subtrees
              topdown {
                matchingChildren {
                  (replaceExpressionWithCall)
                }
              }
          } &>
          insertClosureDef
      }
    }

    Right(transformFile(selection.file, extractClosure))
  }
}