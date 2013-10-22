package scala.tools.refactoring.implementations.extractCode

import scala.tools.refactoring.common.CompilerAccess

trait ExtractionTargets { self: ExtractCode =>
  import global._

  /**
   * Represents an abstraction of the code to extract.
   */
  trait ExtractionTarget {
    def getCodeAndCall(selection: Selection, userInput: RefactoringParameters): (Tree, Tree)
  }

  /**
   *
   */
  case class NewDef(name: String, parameters: Symbol => Boolean) extends ExtractionTarget {
    def getCodeAndCall(selection: Selection, userInput: RefactoringParameters) = {
      (EmptyTree, EmptyTree)
    }
  }

  case class NewVal(name: String) extends ExtractionTarget {
    def getCodeAndCall(selection: Selection, userInput: RefactoringParameters) = {
      (EmptyTree, EmptyTree)
    }
  }

  case class NewDefOrVal(name: String, parameters: Symbol => Boolean) extends ExtractionTarget {
    def getCodeAndCall(selection: Selection, userInput: RefactoringParameters) = {
      (EmptyTree, EmptyTree)
    }
  }
}