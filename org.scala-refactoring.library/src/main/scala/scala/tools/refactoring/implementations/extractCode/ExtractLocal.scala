package scala.tools.refactoring
package implementations.extractCode

import common.Change
import common.CompilerAccess
import analysis.Indexes

abstract class ExtractLocal extends ExtractCodeBase {
  import global._

  case class RefactoringParameters(
    name: String)

  def perform(selection: Selection, preparation: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    extract(selection, NewVal(preparation.targetScopes.last, params.name))
  }
}