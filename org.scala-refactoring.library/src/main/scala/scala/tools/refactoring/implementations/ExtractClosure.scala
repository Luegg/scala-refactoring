package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes

abstract class ExtractClosure extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

  case class PreparationResult(selectedExpr: Tree, owner: DefTree, potentialParameters: List[Symbol])

  case class RefactoringParameters(closureName: String, becomesParamFilter: Symbol => Boolean)

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {
    def afterOrInSelection(p: Position) =
      if (s.pos.start == s.pos.end)
        p.isRange && p.start > s.pos.start
      else
        p.sameRange(s.pos)

    def extractableTree =
      s.root.find {
        case t @ (_: SymTree | _: TermTree) => afterOrInSelection(t.pos)
        case _ => false
      }

    def owner =
      s.findSelectedWithPredicate { t =>
        t match {
          case _: DefDef => true
          case _: ImplDef => true
          case _ => false
        }
      } map (_.asInstanceOf[DefTree])

    extractableTree.map { tree =>
      PreparationResult(tree, owner.get, inboundLocalDependencies(s, owner.get.symbol))
    }.toRight(PreparationError("Selection can't be extracted"))
  }

  def perform(selection: Selection, selectedExpression: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    Left(RefactoringError("not yet implemented"))
  }
}