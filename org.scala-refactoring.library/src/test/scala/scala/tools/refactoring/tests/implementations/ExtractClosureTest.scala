package scala.tools.refactoring
package tests.implementations

import tests.util
import implementations.ExtractClosure
import org.junit.Assert._
import org.junit.Test

class ExtractClosureTest extends util.TestRefactoring {
  import global._

  def extract(closureName: String, closureParams: List[String])(files: FileSet) = new TestRefactoringImpl(files) {
    val refactoring = new ExtractClosure with SilentTracing with TestProjectIndex
    val filter = (sym: refactoring.global.Symbol) => closureParams.contains(sym.nameString)
    val changes = performRefactoring(refactoring.RefactoringParameters(closureName, filter))
  }.changes

  def prepare(fileContent: String) = {
    val files = new FileSet { fileContent becomes "" }
    val refactoringImpl = new TestRefactoringImpl(files) {
      val refactoring = new ExtractClosure with SilentTracing with TestProjectIndex
    }
    val result = refactoringImpl.preparationResult

    new {
      def assertSuccess =
        assertTrue(result.isRight)
      def assertFailure =
        assertTrue(result.isLeft)
    }
  }

  @Test
  def prepareWithSelectedStatementInObject = prepare(
    """
    package extractClosure
    object Demo {
	  val osx = "MAC"
      
	  if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
        println("you're using Mac OsX");
    }
    """).assertSuccess

  @Test
  def prepareWithSelectedStatementInMethod = prepare(
    """
    package extractClosure
    object Demo {
	  val osx = "MAC"
      
      def printOsInfo =
	    if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
          println("you're using Mac OsX");
    }
    """).assertSuccess

  @Test
  def prepareBlock = prepare(
    """
    package extractClosure
    object Demo {
      def printThree = {
        /*(*/val a = 1
		val b = 2/*)*/
        println(a+b)
	  }
    }
    """).assertSuccess

  @Test
  def prepareWithInvalidSelection = prepare(
    """
    package extractClosure
    object Demo {
      /*(*/def printString/*)*/(v: Any) =
        println(v.toString)
    }
    """).assertFailure

  @Test
  def extractSimpleClosure = new FileSet {
    """
	package extractClosure
	object Demo{
	  def printInfo = {
	    println("nonsense")
	    /*(*/println("hi")/*)*/
	  }
	}
	""" becomes
      """
	package extractClosure
	object Demo{
	  def printInfo = {
	    println("nonsense")
	    def greet = {
	      /*(*/println("hi")/*)*/
	    }
	    greet
	  }
	}
	"""
  } applyRefactoring (extract("greet", Nil))

  @Test
  def extractSimpleBlock = new FileSet {
    """
	package extractClosure
	object Demo{
	  def printInfo = {
	    println("nonsense")
	    /*(*/val greeting = "hello"
	    val name = "world"/*)*/
	    println(greeting + name)
	  }
	}
	""" becomes
      """
	package extractClosure
	object Demo{
	  def printInfo = {
	    def mkGreeting ={
	      println("nonsense")
	      /*(*/val greeting = "hello"
	      val name = "world"/*)*/
	      (greeting, name)
	    }
	    val (greeting, name) = mkGreeting
	    println(greeting + name)
	  }
	}
	"""
  } applyRefactoring (extract("mkGreeting", Nil))

  @Test
  def extractClosureFromDef = new FileSet {
    """
	package extractClosure
	object Demo {
	  def printOsInfo(os: String) = {
	    val osx = "MAC"
	
	    if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
	      println("you're using Mac OsX");
	  }
	}
	""" becomes
      """
	package extractClosure
	object Demo {
	  def printOsInfo(os: String) = {
	    val osx = "MAC"
	
	    def isOs(osx: String) = {
	      /*(*/os.toUpperCase.indexOf(osx) != -1/*)*/
	    }
	
	    if(isOs(osx))
	      println("you're using Mac OsX");
	  }
	}
	"""
  } applyRefactoring (extract("isOs", "osx" :: Nil))

  @Test
  def extractClosureFromObject = new FileSet {
    """
	package extractClosure
	object Demo extends App{
	  val os = System.getProperties.get("os.name")
	  val osx = "MAC"
	
	  if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
	    println("you're using Mac OsX");
	}
	""" becomes
      """
	package extractClosure
	object Demo extends App{
	  val os = System.getProperties.get("os.name")
	  val osx = "MAC"
	
	  def isOs(osx: String) = {
	    /*(*/os.toUpperCase.indexOf(osx) != -1/*)*/
	  }
	
	  if(isOs(osx))
	    println("you're using Mac OsX");
	}
	"""
  } applyRefactoring (extract("isOs", "osx" :: Nil))
}