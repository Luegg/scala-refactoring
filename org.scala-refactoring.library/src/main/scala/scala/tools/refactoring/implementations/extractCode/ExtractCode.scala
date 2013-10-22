package scala.tools.refactoring
package implementations.extractCode

abstract class ExtractCode extends ExtractCodeBase {
  import global._

  case class RefactoringParameters(
    targetScope: TargetScope,
    name: String,
    selectedParameters: List[Symbol])

  def perform(selection: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    extract(selection, NewValOrDef(params.targetScope, params.name, params.selectedParameters))
  }
}