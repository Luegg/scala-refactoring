package scala.tools.refactoring
package implementations

import common.Change
import common.CompilerAccess

abstract class ExtractClosure extends MultiStageRefactoring with CompilerAccess{
  import global._

  case class PreparationResult(selectedExpr: Tree, potentialParameters: List[Symbol])
  
  case class RefactoringParameters(closureName: String, becomesParamFilter: Symbol => Boolean)
  
  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {    
    Left(PreparationError("not yet implemented"))
  }
  
  def perform(selection: Selection, selectedExpression: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    Left(RefactoringError("not yet implemented"))
  }
}