/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.OrganizeImports
import tests.util.TestRefactoring
import tests.util.TestHelper

class OrganizeImportsTest extends TestHelper with TestRefactoring {
  outer =>
  
  def organize(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  @Test
  def sort = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.Object
  
      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """ becomes
    """
      import java.lang.Object
      import scala.collection.mutable.ListBuffer
  
      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """
  } applyRefactoring organize
    
  @Test
  def collapse = new FileSet {
    """
      import java.lang.String
      import java.lang.Object
  
      object Main {val s: String = ""; var o: Object = null}
    """ becomes
    """
      import java.lang.{Object, String}
  
      object Main {val s: String = ""; var o: Object = null}
    """
  } applyRefactoring organize
    
  @Test
  def sortAndCollapse = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.String
      import java.lang.Object
  
      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """ becomes
    """
      import java.lang.{Object, String}
      import scala.collection.mutable.ListBuffer
  
      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """
  } applyRefactoring organize
    
  @Test
  def collapseWithRename = new FileSet {
    """
      import java.lang.{String => S}
      import java.lang.{Object => Objekt}
  
      object Main {val s: String = ""; var o: Objekt = null}
    """ becomes
    """
      import java.lang.{Object => Objekt}
  
      object Main {val s: String = ""; var o: Objekt = null}
    """
  } applyRefactoring organize
    
  @Test
  def removeOneFromMany = new FileSet {
    """
      import java.lang.{String, Math}
  
      object Main {val s: String = ""}
    """ becomes
    """
      import java.lang.{String}
  
      object Main {val s: String = ""}
    """
  } applyRefactoring organize
    
  @Test
  def importAll = new FileSet {
    """
      import java.lang._
      import java.lang.String
  
      object Main
    """ becomes
    """
      import java.lang._
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def importOnTrait = new FileSet {
    """
      package importOnTrait
      import java.lang._
      import java.lang.String
  
      trait A
  
      trait Main extends A {
      }
    """ becomes
    """
      package importOnTrait
      import java.lang._
  
      trait A
  
      trait Main extends A {
      }
    """
  } applyRefactoring organize
    
  @Test
  def importWithSpace = new FileSet {
    """
  
      import scala.collection.mutable.ListBuffer
      import java.lang.String
  
      object Main { val s: String = ""; val lb = ListBuffer("") }
    """ becomes
    """
  
      import java.lang.String
      import scala.collection.mutable.ListBuffer
  
      object Main { val s: String = ""; val lb = ListBuffer("") }
    """
  } applyRefactoring organize
    
  @Test
  def importAllWithRename = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}
  
      object Main { val s: String = "" }
    """ becomes
    """
      import java.lang.{String => S, _}
  
      object Main { val s: String = "" }
    """
  } applyRefactoring organize
    
  @Test
  def importRemovesUnneeded = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}
      import java.util.Map
      import scala.io.Source
      import scala.collection.mutable.ListBuffer

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """ becomes
    """
      import java.lang.{String => S, _}
      import scala.collection.mutable.ListBuffer

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """
  } applyRefactoring organize
    
  @Test
  def multipleImportsOnOneLine = new FileSet {
    """
      import java.lang.String, String._
  
      object Main {
        val s: String = ""
      }    """ becomes
    """
      import String._
      import java.lang.String
  
      object Main {
        val s: String = ""
      }    """
  } applyRefactoring organize
}