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

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {
    if (s.selectedTopLevelTrees.size > 0)
      s.findSelectedWithPredicate {
        case _: DefDef | _: Function | _: CaseDef | _: Block => true
        case _ => false
      } match {
        case Some(t) => Right(PreparationResult(t, inboundDependencies(s)))
        case None => Left(PreparationError("Can't extract closure from this position."))
      }
    else
      Left(PreparationError("No expression or statement selected."))
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val params = preparation.potentialParameters.filter(userInput.closureParameters(_))
    val returns = outboundLocalDependencies(selection)

    val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
    val closure = mkDefDef(
      NoMods,
      userInput.closureName,
      if (params.isEmpty) Nil else params :: Nil,
      selection.selectedTopLevelTrees ::: returnStatement)

    val call = mkCallDefDef(userInput.closureName, params :: Nil, returns)

    val extractSingleStatement = selection.selectedTopLevelTrees.size == 1

    val replaceExpressionWithCall =
      if (extractSingleStatement)
        replaceTree(selection.selectedTopLevelTrees.head, call)
      else
        fail[Tree]

    val replaceBlockWithCall =
      transform {
        case block @ BlockExtractor(stats) if stats.size > 0 => {
          val newStats = stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)
          mkBlock(newStats) replaces block
        }
      }

    val findEnclosingTree = predicate((t: Tree) => t == preparation.enclosingTree)

    val insertClosureDef = transform {
      case t @ Block(stats, expr) => {
        val (before, after) = stats.span { t =>
          t.pos.isRange && t.pos.end <= selection.pos.start
        }
        t copy (stats = before ::: closure :: after)
      }
      case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
        t copy (rhs = Block(closure :: Nil, rhs))
      case t @ Function(_, body) =>
        t copy (body = Block(closure :: Nil, body))
      case t @ CaseDef(_, _, body) =>
        t copy (body = Block(closure :: Nil, body))
    }

    val extractClosure = topdown {
      matchingChildren {
        findEnclosingTree &>
        {
          // first try to replace direct children
          replaceBlockWithCall |> replaceExpressionWithCall |>
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