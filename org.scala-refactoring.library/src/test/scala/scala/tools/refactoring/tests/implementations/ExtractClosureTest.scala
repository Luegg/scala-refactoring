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
        /*(*/println("hello")
        println("world")/*)*/
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def printInfo = {
        println("nonsense")
        def greet: Unit = {
          /*(*/println("hello")
          println("world")/*)*/
        }
        greet
      }
    }
    """
  } applyRefactoring (extract("greet", Nil))

  @Test
  def extractBlockWith2OutboundDependencies = new FileSet {
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
  def extractExpressionWithParams = new FileSet {
    """
    package extractClosure
    object Demo {
      def calc = {
        val (a, b, c) = (1, 2, 3)
        /*(*/println(a + b + c)/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo {
      def calc = {
        val (a, b, c) = (1, 2, 3)
        def printRes(a: Int, b: Int) = {
          /*(*/println(a + b + c)/*)*/
        }
        printres
      }
    }
    """
  } applyRefactoring(extract("printRes", "a" :: "b" :: Nil))

  @Test
  def extractBlockWith1ParamUsedTwice = new FileSet {
    """
    package extractClosure
    object Demo {
      def printOsInfo(os: String) = {
        val osx = "MAC"
        /*(*/if(os.toUpperCase.indexOf(osx) != -1)
        println("you're using " + osx);/*)*/
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo {
      def printOsInfo(os: String) = {
        val osx = "MAC"
        def printOsIfEqual(osx: String): Boolean = {
          /*(*/if(os.toUpperCase.indexOf(osx) != -1)
          println("you're using Mac OsX");/*)*/
        }
    
        printOsIfEqual(osx)
      }
    }
    """
  } applyRefactoring (extract("printOsIfEqual", "osx" :: Nil))
  
  @Test
  def extractFromInnerDef = new FileSet {
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
          i => 
            def extracted: Unit = {
              /*(*/println(i)/*)*/
            }
            extracted
        }
      }
    }
    """
  } applyRefactoring(extract("extracted", Nil))
  
  @Test
  def extractFromCase = new FileSet{
    """
    package extractClosure
    object Demo{
      1 match{
        case a: Int => /*(*/println(a)/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      1 match{
        case a: Int => 
          def out(a: Int) = {
            /*(*/println(a)/*)*/
          }
          out(a)
      }
    }
    """
  } applyRefactoring(extract("out", "a" :: Nil))
  
  @Test
  def extractFromNonBlock = new FileSet{
    """
    package extractClosure
    object Demo{
      def calc(a: Int) = {
        if(a <= 7)
          if(a >= 3)
            /*(*/3 * a/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def calc(a: Int) = {
        def extracted(a: Int) = {
          /*(*/3 * a/*)*/
        }
        if(a <= 7)
          if(a >= 3)
            extracted(a)
      }
    }
    """
  } applyRefactoring(extract("extracted", "a" :: Nil))
  
  @Test
  def extractFromForEnumerator = new FileSet{
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(
            i <- /*(*/a :: Nil/*)*/
          ) yield i
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(a: Int): List[Int] = {
          /*(*/a :: Nil/*)*/
        }
        for(
            i <- extracted(a)
          ) yield i
      }
    }
    """
  } applyRefactoring(extract("extracted", "a" :: Nil))
  
  @Test
  def extractFromYield = new FileSet{
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(
            i <- 1 to 10
          ) yield /*(*/i * a/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(
            i <- 1 to 10
          ) yield {
          def extracted(a: Int) = {
            /*(*/i * a/*)*/
          }
          extracted(a)
        }
      }
    }
    """
  } applyRefactoring(extract("extracted", "a" :: Nil))
}