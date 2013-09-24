/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

abstract class ExplicitGettersSetters extends MultiStageRefactoring with ParameterlessRefactoring with common.InteractiveScalaCompiler {

  import global._

  type PreparationResult = ValDef

  def prepare(s: Selection) = {
    s.findSelectedOfType[ValDef] match {
      case Some(valdef) => Right(valdef)
      case None => Left(new PreparationError("no valdef selected"))
    }
  }

  override def perform(selection: Selection, selectedValue: PreparationResult): Either[RefactoringError, List[Change]] = {

    val template = selection.findSelectedOfType[Template].getOrElse {
      return Left(RefactoringError("no template found"))
    }

    val createSetter = selectedValue.symbol.isMutable
    val publicName = newTermName(selectedValue.name.toString.trim)
    val privateName = newTermName("_" + publicName)

    val privateFieldMods = if (createSetter)
      Modifiers(Flags.PARAMACCESSOR).
        withPosition(Flags.PRIVATE, NoPosition).
        withPosition(Tokens.VAR, NoPosition)
    else
      Modifiers(Flags.PARAMACCESSOR)

    val privateField = selectedValue copy (mods = privateFieldMods, name = privateName)

    implicit def blockify(t: Tree) = new {
      def asBlock = Block(t :: Nil, EmptyTree)
    }

    def getter = q"""
      def $publicName = {
    	${Ident(privateName).asBlock}
      }
      """ copy (mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition))

    def setter = {
      val setterName = newTermName(publicName + "_=")
      val valType = selectedValue.tpt.tpe
      q"""
      def $setterName($publicName: $valType) = {
      	${q"$privateName = $publicName".asBlock}
      }
      """ copy (mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition))
    }

    val insertGetterSettersTransformation = transform {

      case tpl: Template if tpl == template =>

        val classParameters = tpl.body.map { t =>
          if (t == selectedValue) privateField setPos t.pos
          else t
        }

        val body = if (createSetter)
          getter :: setter :: classParameters
        else
          getter :: classParameters

        tpl.copy(body = body) setPos tpl.pos
    }

    Right(transformFile(selection.file, topdown(matchingChildren(insertGetterSettersTransformation))))
  }
}
