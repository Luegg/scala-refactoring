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
  def prepareExpressionInObjectNotDefiningNewMembers = prepare(
    """
    package extractClosure
    object Demo {
      val os = "abc"
      val osx = "MAC"
      
      if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
        println("you're using Mac OsX");
    }
    """).assertSuccess

  @Test
  def prepareExpressionsDefiningNewMembers = prepare(
    """
    package extractClosure
    object Demo {
      /*(*/val a = 1/*)*/
    }
    """).assertFailure

  @Test
  def prepareExpressionInMethod = prepare(
    """
    package extractClosure
    object Demo {
      val os = "abc"
      val osx = "MAC"
      
      def printOsInfo =
        if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
          println("you're using Mac OsX");
    }
    """).assertSuccess

  @Test
  def prepareSeqOfExpressions = prepare(
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
  def prepareWithInevitableParameter = prepare(
    """
    package extractClosure
    object Demo {
      def a = {
        for(i <- 1 to 10)/*(*/{
          println(i)
        }/*)*/
	  }
    }
    """).assertSuccess

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
  }.refactor(extract("greet", Nil)).assertEqualTree

  @Test
  def extractSimpleExpressions = new FileSet {
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
  }.refactor(extract("greet", Nil)).assertEqualTree
  
  @Test
  def extractClosureWithInevitableParameter = new FileSet{
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(i <- 1 to 10)/*(*/{
          println(i)
        }/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(i: Int): Unit = {
          println(i)
        }
        for(i <- 1 to 10){
          extracted(i)
        }
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree
  
  @Test
  def extractBlock = new FileSet{
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(i <- 1 to 10)/*(*/{
          println("block!")
          println(i)
        }/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(i: Int): Unit = {
          println("block!")
          println(i)
        }
        for(i <- 1 to 10){
          extracted(i)
        }
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree

  @Test
  def extractExpressionsWith2OutboundDependencies = new FileSet {
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
  }.refactor(extract("mkGreeting", Nil)).assertEqualTree
  
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
        def printRes(a: Int, b: Int): Unit = {
          /*(*/println(a + b + c)/*)*/
        }
        printRes(a, b)
      }
    }
    """
  }.refactor(extract("printRes", "a" :: "b" :: Nil)).assertEqualTree

  @Test
  def extractExpressionsWith1ParamUsedTwice = new FileSet {
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
        def printOsIfEqual(osx: String): Unit = {
          /*(*/if(os.toUpperCase.indexOf(osx) != -1)
          println("you're using Mac OsX");/*)*/
        }
    
        printOsIfEqual(osx)
      }
    }
    """
  }.refactor(extract("printOsIfEqual", "osx" :: Nil)).assertEqualTree

  @Test
  def extractExpressionWithOutboundDepUsedTwice = new FileSet {
    """
    package extractClosure
    object Demo {
      def calc = {
        /*(*/val a = 1/*)*/
        a + a
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo {
      def calc = {
        def extracted: Int = {
          /*(*/val a = 1/*)*/
          a
        }
        val a = extracted
        a + a
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree
  
  @Test
  def extractFromObject = new FileSet {
    """
    package extractClosure
    object Demo{
      if(/*(*/100 < 1000/*)*/)
        println("Math not broken")
	}
    """ becomes """
    package extractClosure
    object Demo{
      def extracted = {
        /*(*/100 < 1000/*)*/
      }
      if(extracted)
        println("Math not broken")
	}
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree
  
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
  }.refactor(extract("extracted", "name" :: Nil)).assertEqualTree
  
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
  }.refactor(extract("extracted", Nil)).assertEqualTree
  
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
          def out(a: Int): Unit = {
            /*(*/println(a)/*)*/
          }
          out(a)
      }
    }
    """
  }.refactor(extract("out", "a" :: Nil)).assertEqualTree
  
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
        def extracted(a: Int): Int = {
          /*(*/3 * a/*)*/
        }
        if(a <= 7)
          if(a >= 3)
            extracted(a)
      }
    }
    """
  }.refactor(extract("extracted", "a" :: Nil)).assertEqualTree
  
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
  }.refactor(extract("extracted", "a" :: Nil)).assertEqualTree
  
  @Test
  def extractFromYield = new FileSet{
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(i <- 1 to 10) yield /*(*/i * a/*)*/
      }
    }
    """ becomes
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(i: Int): Int = {
          /*(*/i * a/*)*/
        }
        for(i <- 1 to 10) yield extracted(i)
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree
}