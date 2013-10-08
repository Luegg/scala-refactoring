package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.Flags

abstract class ExtractClosure extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

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
    /** defines for each optional parameter if it's used as a closure parameter */
    closureParameters: Symbol => Boolean)

  def prepare(selection: Selection): Either[PreparationError, PreparationResult] = {

    def mkPreparationResult(enclosingTree: Tree) = {
      val childContainingSelection = enclosingTree.filter { t =>
        t != enclosingTree &&
          t.pos.isRange && t.pos.start < selection.pos.start &&
          t.exists(p => p == selection.allSelectedTrees.head)
      }.headOption.getOrElse(EmptyTree)

      val deps = inboundDependencies(selection).distinct
      val newSymbolsBetweenEnclosingAndSelection = childContainingSelection.filter {
        case t: DefTree => !selection.contains(t)
        case _ => false
      }.map(_.symbol)

      val (requiredParams, optionalParams) = deps.partition { sym =>
        newSymbolsBetweenEnclosingAndSelection.contains(sym)
      }

      Right(PreparationResult(enclosingTree, optionalParams, requiredParams))
    }

    val definesNonLocal = selection.selectedSymbols.exists { sym =>
      !sym.isLocal && index.declaration(sym).exists(selection.contains(_))
    }

    if (definesNonLocal)
      Left(PreparationError("Can't extract expressions that defines non local fields"))
    else if (selection.selectedTopLevelTrees.size > 0)
      selection.findSelectedWithPredicate { // find a tree to insert the closure definition
        case b: Block if b == selection.selectedTopLevelTrees.head => false
        case _: DefDef | _: Block | _: Template => true
        case _ => false
      }.map(mkPreparationResult(_)).getOrElse(Left(PreparationError("Can't extract closure from this position.")))
    else
      Left(PreparationError("No expression or statement selected."))
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val params = inboundDependencies(selection).distinct.filter { dep =>
      preparation.requiredParameters.contains(dep) ||
        preparation.optionalParameters.filter(userInput.closureParameters(_)).contains(dep)
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
        case block @ BlockExtractor(stats) if stats.size > 0 => {
          val newStats = stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)
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

  private def treeEnclosingSelection(selection: ExtractClosure.this.Selection): Option[ExtractClosure.this.global.Tree] = {
    selection.findSelectedWithPredicate { t =>
      t.pos.isRange && t.pos.start < selection.pos.start && t.pos.end > selection.pos.end
    }
  }

  private def defDefWithName(closureName: String, enclosingTree: Option[Tree]): Option[Tree] = {
    enclosingTree.getOrElse(EmptyTree).find {
      case d @ DefDef(_, name, _, _, _, _) =>
        name.decode == closureName
      case _ => false
    }
  }

  private def occurrence(p: Position) =
    (p.start, p.end - p.start)

  def getClosureNameOccurences(refactoredSelection: Selection, userInput: RefactoringParameters): List[(Int, Int)] = {
    val enclosingTree = treeEnclosingSelection(refactoredSelection)
    val closureDefDef = defDefWithName(userInput.closureName, enclosingTree)

    closureDefDef match {
      case Some(d) =>
        val defOccurrences = occurrence(d.namePosition())
        val refOccurrences = index.references(d.symbol).map {
          ref => occurrence(ref.pos)
        }
        defOccurrences :: refOccurrences
      case None => Nil
    }
  }

  def getClosureParamsOccurences(refactoredSelection: Selection, userInput: RefactoringParameters): List[List[(Int, Int)]] = {
    val enclosingTree = treeEnclosingSelection(refactoredSelection)
    val closureDefDef = defDefWithName(userInput.closureName, enclosingTree)

    closureDefDef match {
      case Some(DefDef(_, _, _, params, _, _)) =>
        params.flatten.map { p =>
          val pOccurrence = occurrence(p.namePosition())
          val refOccurrences = index.references(p.symbol).map {
            ref => occurrence(ref.pos)
          }
          pOccurrence :: refOccurrences
        }
      case _ => Nil
    }
  }
}