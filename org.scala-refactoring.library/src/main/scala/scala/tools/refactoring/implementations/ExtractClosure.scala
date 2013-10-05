package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.Flags
import PartialFunction._

abstract class ExtractClosure extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

  case class PreparationResult(
    enclosingTree: Tree,
    potentialParameters: List[Symbol],
    inevitableParameters: List[Symbol])

  case class RefactoringParameters(closureName: String, closureParameters: Symbol => Boolean)

  def inboundDependencies(selection: Selection): List[Symbol] = {
    for (
      selected <- selection.selectedSymbols;
      declaration <- index.declaration(selected) if !selection.contains(declaration) && selected.pos.isOpaqueRange
    ) yield selected
  }

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {

    def findChildContainingSelection(enclosingTree: Tree) = {
      val filterer = new FilterTreeTraverser(cond(_) {
        case t if t != enclosingTree =>
          t.pos.isRange && t.pos.start < s.pos.start && t.exists(p => p == s.allSelectedTrees.head)
      })
      filterer.traverse(enclosingTree)
      filterer.hits.headOption
    }

    def preparationSuccess(enclosingTree: Tree) = {
      val deps = inboundDependencies(s).distinct
      val newSymbolsBetweenEnclosingAndSelection = findChildContainingSelection(enclosingTree) match {
        case Some(t) => t.filter {
          case t: DefTree => !s.contains(t)
          case _ => false
        }.map(_.symbol)
        case None => List()
      }

      val (inevitableParams, potentialParams) = deps.partition { sym => newSymbolsBetweenEnclosingAndSelection.contains(sym) }

      Right(PreparationResult(enclosingTree, potentialParams, inevitableParams))
    }

    val definesNonLocal = s.selectedSymbols.exists { sym =>
      !sym.isLocal && (index.declaration(sym) match {
        case Some(t) => s.contains(t)
        case None => false
      })
    }

    if (definesNonLocal)
      Left(PreparationError("Can't extract expression that defines non local fields"))
    else if (s.selectedTopLevelTrees.size > 0)
      s.findSelectedWithPredicate { // find a tree to insert the closure definition
        case b: Block if b == s.selectedTopLevelTrees.head => false
        case _: DefDef | _: Function | _: CaseDef | _: Block | _: Template => true
        case _ => false
      } match {
        case Some(t) => preparationSuccess(t)
        case None => Left(PreparationError("Can't extract closure from this position."))
      }
    else
      Left(PreparationError("No expression or statement selected."))
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val params = preparation.potentialParameters.filter(userInput.closureParameters(_)) ::: preparation.inevitableParameters
    val returns = outboundLocalDependencies(selection).distinct

    val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
    val closureBody = selection.selectedTopLevelTrees match {
      // a single block tree could be unpacked
      case Block(stats, expr) :: Nil => stats ::: expr :: returnStatement
      case stats => stats ::: returnStatement
    }
    val closure = mkDefDef(
      NoMods,
      userInput.closureName,
      if (params.isEmpty) Nil else params :: Nil,
      closureBody)

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
        t copy (stats = before ::: closure :: after) replaces t
      }
      case t @ Template(_, _, body) =>
        val (before, after) = body.span { t =>
          t.pos.isRange && t.pos.end <= selection.pos.start
        }
        t copy (body = before ::: closure :: after) replaces t
      case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
        t copy (rhs = Block(closure :: Nil, rhs)) replaces t
      case t @ Function(_, body) =>
        t copy (body = Block(closure :: Nil, body)) replaces t
      case t @ CaseDef(_, _, body) =>
        t copy (body = Block(closure :: Nil, body)) replaces t
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