package scala.tools.refactoring.implementations.extractCode

import scala.tools.refactoring.common.CompilerAccess
import scala.reflect.internal.Flags

trait ExtractionTargets { self: ExtractCode =>
  import global._

  /**
   * Represents an abstraction of the code to extract.
   */
  trait ExtractionTarget {
    val targetScope: TargetScope

    val name: String

    val selectedParameters: List[Symbol]

    def getCodeAndCall(selection: Selection): Either[RefactoringError, (Tree, Tree)]

    protected def parameters(selection: Selection) =
      inboundDependencies(selection)
        .filter { dep =>
          targetScope.requiredParameters.contains(dep) ||
            selectedParameters.contains(dep)
        }

    protected def returnValues(selection: Selection) =
      outboundLocalDependencies(selection).distinct

    protected def body(selection: Selection) = {
      val returns = returnValues(selection)
      val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
      selection.selectedTopLevelTrees match {
        // a single block tree could be unpacked
        case Block(stats, expr) :: Nil => stats ::: expr :: returnStatement
        case stats => stats ::: returnStatement
      }
    }

    protected val mods = targetScope.tree match {
      case _: Template => NoMods withPosition (Flags.PRIVATE, NoPosition)
      case _ => NoMods
    }

    protected def defDefWithCall(selection: Selection) = {
      val params = parameters(selection)
      val method = mkDefDef(
        mods,
        name,
        if (params.isEmpty) Nil else params :: Nil,
        body(selection))
      val call = mkCallDefDef(name, params :: Nil, returnValues(selection))

      (method, call)
    }

    protected def valDefWithCall(selection: Selection) = {
      if (targetScope.requiredParameters.isEmpty) {
        Right((mkValDef(name, selection.selectedTopLevelTrees.head), Ident(name)))
      } else {
        Left(RefactoringError(s"Can't extract expression with inbound dependencies ${targetScope.requiredParameters}"))
      }
    }
  }

  /**
   *
   */
  case class NewDef(targetScope: TargetScope, name: String, selectedParameters: List[Symbol]) extends ExtractionTarget {
    def getCodeAndCall(selection: Selection) =
      Right(defDefWithCall(selection))
  }

  case class NewVal(targetScope: TargetScope, name: String) extends ExtractionTarget {
    val selectedParameters = Nil

    def getCodeAndCall(selection: Selection) =
      valDefWithCall(selection)

  }

  case class NewDefOrVal(targetScope: TargetScope, name: String, selectedParameters: List[Symbol]) extends ExtractionTarget {
    def getCodeAndCall(selection: Selection) =
      if (parameters(selection).isEmpty)
        Right((EmptyTree, EmptyTree))
      else
        Right(defDefWithCall(selection))
  }
}