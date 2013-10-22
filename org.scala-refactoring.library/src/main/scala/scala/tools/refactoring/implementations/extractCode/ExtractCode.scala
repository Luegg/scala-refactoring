package scala.tools.refactoring
package implementations.extractCode

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.Flags
import scala.tools.refactoring.MultiStageRefactoring

abstract class ExtractCode extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess with ExtractionTargets {
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

  object TargetScope {
    /**
     * Constructs TargetScope from the scope represented by `tree` and a selection
     * which marks the code to extract
     */
    def apply(tree: Tree, selection: Selection): TargetScope = {
      val (opt, req) = getParameters(tree, selection)
      TargetScope(tree, opt, req)
    }

    private def getParameters(tree: Tree, selection: Selection) = {
      val childContainingSelection = tree.filter { t =>
        t != tree &&
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

      (optionalParams, requiredParams)
    }
  }

  case class PreparationResult(
    targetScopes: List[TargetScope])

  type RefactoringParameters = ExtractionTarget

  private def findTargetScopes(selection: Selection) = {
    val scopes = selection.filterSelectedWithPredicate {
      case b: Block if b == selection.selectedTopLevelTrees.head => false
      case DefDef(_, _, _, _, _, _: Block) => false // if def has block body, only use body as target scope
      case _: DefDef | _: Block | _: Template => true
      case _ => false
    }

    scopes.map(TargetScope(_, selection))
  }

  def prepare(selection: Selection): Either[PreparationError, PreparationResult] = {
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
      findTargetScopes(selection) match {
        case Nil => Left(PreparationError("Can't extract closure from this position."))
        case scopes => Right(PreparationResult(scopes))
      }
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    userInput.getCodeAndCall(selection).right.map { (codeAndCall) =>
      val (closure, call) = codeAndCall
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

      val findEnclosingTree = predicate { (t: Tree) => t == userInput.targetScope.tree }

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

      transformFile(selection.file, extractClosure)
    }
  }
}