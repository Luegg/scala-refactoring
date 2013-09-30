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
    val declarationsInTheSelection = selection.selectedSymbols filter (s => index.declaration(s).map(selection.contains) getOrElse false)
    val occurencesOfSelectedDeclarations = declarationsInTheSelection flatMap (index.occurences)
    occurencesOfSelectedDeclarations.filterNot(selection.contains).map(_.symbol).distinct
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
    val closureName = newTermName(userInput.closureName)
    val params = preparation.potentialParameters.filter(userInput.closureParameters(_))
    val returns = outboundDependencies(selection)

    val closure = {
      val paramVals = params.map(p => q"val ${TermName(p.nameString)}: ${p.tpe}") :: Nil
      val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
      q"""
      def $closureName(...$paramVals) = {
      	..${selection.selectedTopLevelTrees}
        ..$returnStatement
      }
      """.copy(mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition))
    }

    val call = {
      val args = params.map(p => Ident(p)) :: Nil
      returns match {
        case Nil => q"$closureName(...$args)"
        case r :: Nil => q"val r = $closureName(...$args)"
        case rs => {
          val tupple = newTermName("(" + (rs map (_.name) mkString ", ") + ")")
          q"val $tupple = $closureName(...$args)"
        }
      }
    }

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