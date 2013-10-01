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
    """).assertFailure

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
  def extractSimpleExpression = new FileSet {
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
        def greet: Unit = {
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
	    println("nonsense")
	    def mkGreeting: (String, String) = {
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
  def extractExpressionWithParam = new FileSet {
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
	    def isOs(osx: String): Boolean = {
	      /*(*/os.toUpperCase.indexOf(osx) != -1/*)*/
		}
	
	    if(isOs(osx))
	      println("you're using Mac OsX");
	  }
	}
	"""
  } applyRefactoring (extract("isOs", "osx" :: Nil))
  
  @Test
  def extractFromClosure = new FileSet {
    """
    package extractClosure
    object Demo{
      def printInfo = {
        println("nonsense")
        def greet(name: String) =
          /*(*/println("hello " + name)/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def printInfo = {
        println("nonsense")
        def greet(name: String) = {
          def extracted(name: String): Unit = {
            /*(*/println("hello " + name)/*)*/
    	  }
          extracted(name)
        }
      }
    }
    """
  } applyRefactoring(extract("extracted", "name" :: Nil))
  
  @Test
  def extractFromAnonymousFunction = new FileSet {
    """
    package extractClosure
    object Demo{
      def printInfo = {
        println("nonsense")
        (1 to 9).foreach{
          i => /*(*/println(i)/*)*/
        }
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def printInfo = {
        println("nonsense")
        (1 to 9).foreach{
          i => {
            def extracted: Unit = {
              /*(*/println(i)/*)*/
            }
            extracted
          }
        }
      }
    }
    """
  } applyRefactoring(extract("extracted", Nil))
}