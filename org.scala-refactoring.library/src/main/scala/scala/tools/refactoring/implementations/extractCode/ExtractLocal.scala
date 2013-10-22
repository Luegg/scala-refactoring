package scala.tools.refactoring
package implementations.extractCode

import common.Change
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.analysis.Indexes

abstract class ExtractLocal extends ExtractCode{
  import global._
  
  type RefactoringParameter = String
  
  def perform(selection: Selection, preparation: PreparationResult, name: RefactoringParameter): Either[RefactoringError, List[Change]] = {
    super.perform(selection, preparation, NewVal(preparation.targetScopes.last, name))
  }
}