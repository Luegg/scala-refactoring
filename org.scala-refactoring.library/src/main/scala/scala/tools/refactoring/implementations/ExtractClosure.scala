package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.Flags

abstract class ExtractClosure extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

  case class PreparationResult(enclosingTree: Tree, potentialParameters: List[Symbol])

  case class RefactoringParameters(closureName: String, closureParameters: Symbol => Boolean)

  def inboundDependencies(selection: Selection): List[Symbol] = {
    for (
      selected <- selection.selectedSymbols;
      declaration <- index.declaration(selected) if !selection.contains(declaration) && selected.pos.isOpaqueRange
    ) yield selected
  }

  def outboundDependencies(selection: Selection): List[Symbol] = {
	outboundLocalDependencies(selection, null)
  }

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {
    if (s.selectedTopLevelTrees.size > 0)
      s.findSelectedOfType[DefDef] match{
      	case Some(t) => Right(PreparationResult(t, inboundDependencies(s)))
      	case None => Left(PreparationError("Can't extract closure from this position."))
      }
    else
      Left(PreparationError("No expression or statement selected."))
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val params = preparation.potentialParameters.filter(userInput.closureParameters(_))
    val returns = outboundDependencies(selection)

    val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
    val closure = mkDefDef(NoMods, userInput.closureName, params :: Nil, selection.selectedTopLevelTrees ::: returnStatement)

    val call = mkCallDefDef(userInput.closureName, params :: Nil, returns)

    val extractSingleStatement = selection.selectedTopLevelTrees.size == 1

    val replaceExpression =
      if (extractSingleStatement)
        replaceTree(selection.selectedTopLevelTrees.head, call)
      else
        fail[Tree]

    val replaceBlock =
      transform {
        case block @ BlockExtractor(stats) if stats.size > 0 => {
          val newStats = stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)
          mkBlock(newStats) replaces block
        }
      }

    println(preparation.enclosingTree)
    val findEnclosingTree = predicate((t: Tree) => t == preparation.enclosingTree)

    val insertClosureDef = transform {
      case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
        t copy (rhs = Block(closure :: Nil, rhs))
      case t @ DefDef(_, _, _, _, _, Block(stats, expr)) => {
        val (before, after) = stats.span { t =>
          t.pos.isRange && t.pos.point <= selection.pos.start
        }
        t copy (rhs = Block(before ::: closure :: after, expr))
      }
    }

    val extractClosure = topdown {
      matchingChildren {
        findEnclosingTree &>
        topdown {
          matchingChildren {
            (replaceBlock |> replaceExpression)
          }
        } &>
        insertClosureDef
      }
    }

    Right(transformFile(selection.file, extractClosure))
  }
}