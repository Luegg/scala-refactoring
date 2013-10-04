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
    
	def preparationSuccess(enclosingTree: Tree) = {
	  val symbolsInEnclosingTree = enclosingTree.filter{
	    case _: SymTree => true
	    case _ => false
	  }.map(_.symbol).distinct
	  val selectedSymbols = inboundDependencies(s).distinct
	  
	  val (inevitableParams, potentialParams) = selectedSymbols.partition{ sym => symbolsInEnclosingTree.contains(sym) }
	  
	  Right(PreparationResult(enclosingTree, potentialParams, inevitableParams))
	}
	
    if (s.selectedTopLevelTrees.size > 0)
      s.findSelectedWithPredicate { // find a tree to insert the closure definition
      	case b: Block if b == s.selectedTopLevelTrees.head => false
        case _: DefDef | _: Function | _: CaseDef | _: Block => true
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
    val closureBody = selection.selectedTopLevelTrees match{
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