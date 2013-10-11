package scala.tools.refactoring.common

import scala.tools.refactoring.analysis.Indexes

/**
 * Provides functionalities to get positions of term names. This includes the term name
 * defintion and all its uses.
 */
trait Occurrences extends Selections with CompilerAccess with Indexes {
  import global._
  
  type Occurrence = (Int, Int)

  private def termNameDefinition(selection: Selection, name: String) = {
    selection.enclosingTree.collect {
      case t: DefTree if t.pos.isRange && t.name.decode == name => t
      case t: DefTree if t.name.decode == name => println(t); t
    }.headOption
  }
  
  def toOccurrence(pos: Position): Occurrence =
    (pos.start, pos.end - pos.start)

  /**
   * Returns all uses of the term name introduced by the DefTree t.
   */
  def allOccurrences(t: DefTree): List[Occurrence] = {
    val defOccurrences = toOccurrence(t.namePosition())
    val refOccurrences = index.references(t.symbol).map {
      ref => toOccurrence(ref.pos)
    }
    defOccurrences :: refOccurrences
  }

  /**
   * Searches for a definition in the selection and
   * returns the occurrences of it and all its
   * references or an empty list if definition not found.
   */
  def termNameOccurrences(selection: Selection, name: String): List[Occurrence] = {
    termNameDefinition(selection, name) match {
      case Some(t) => allOccurrences(t)
      case None => Nil
    }
  }

  def defDefParameterOccurrences(selection: Selection, defName: String): List[List[Occurrence]] = {
    termNameDefinition(selection, defName) match {
      case Some(DefDef(_, _, _, params, _, _)) =>
        params.flatten.map { p => allOccurrences(p) }
      case _ => Nil
    }
  }
}