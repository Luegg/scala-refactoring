package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes

abstract class ExtractClosure extends MultiStageRefactoring with TreeAnalysis with Indexes with CompilerAccess {
  import global._

  case class PreparationResult(potentialParameters: List[Symbol])

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
      Right(PreparationResult(inboundDependencies(s)))
    else
      Left(PreparationError("No expression or statement selected."))
  }

  def perform(selection: Selection, preparation: PreparationResult, userInput: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val closure = {
      val params = preparation.potentialParameters.filter(userInput.closureParameters(_))
        .map(p => q"val ${TermName(p.nameString)}: ${p.tpe}")
      val returns = outboundDependencies(selection)
      val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
      q"""
      def ${newTermName(userInput.closureName)}(..$params) = {
      	..${selection.selectedTopLevelTrees}
      	..$returnStatement
      }
      """
    }
    Left(RefactoringError("not yet implemented"))
  }
}