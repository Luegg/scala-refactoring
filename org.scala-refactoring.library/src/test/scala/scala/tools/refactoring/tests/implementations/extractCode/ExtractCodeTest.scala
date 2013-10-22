package scala.tools.refactoring
package tests.implementations.extractCode

import tests.util
import scala.tools.refactoring.implementations.extractCode.ExtractCode
import org.junit.Assert._

class ExtractCodeTest extends util.TestRefactoring {
  import global._

  /**
   * Performs the ExtractCode refactoring by selecting the innermost target scope.
   */
  def extract(closureName: String, closureParams: List[String])(files: FileSet) = new TestRefactoringImpl(files) {
    val refactoring = new ExtractCode with SilentTracing with TestProjectIndex
    val targetScope = preparationResult.right.get.targetScopes.last
    val selectedParams = targetScope.optionalParameters.filter(p => {
      closureParams.contains(p.nameString)
    })
    val refactoringParameters =
      refactoring.NewDef(
          targetScope,
          closureName,
          selectedParams)
    val changes = performRefactoring(refactoringParameters)
  }.changes

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
  }.refactor(extract("greet", Nil)).assertEqualCode

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
  }.refactor(extract("greet", Nil)).assertEqualCode

  @Test
  def extractClosureWithInevitableParameter = new FileSet {
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for(i <- 1 to 10)
          /*(*/println(i)/*)*/
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(i: Int): Unit = {
          /*(*/println(i)/*)*/
        }
        for(i <- 1 to 10){
          extracted(i)
        }
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree

  @Test
  def extractBlock = new FileSet {
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
  }.refactor(extract("printRes", "a" :: "b" :: Nil)).assertEqualCode

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
          println("you're using " + osx);/*)*/
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
  }.refactor(extract("extracted", Nil)).assertEqualCode

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
      private def extracted: Boolean = {
        /*(*/100 < 1000/*)*/
      }

      if(extracted)
        println("Math not broken")
	}
    """
  }.refactor(extract("extracted", Nil)).assertEqualCode

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
        def extracted(i: Int): Unit = {
          /*(*/println(i)/*)*/
        }
        (1 to 9).foreach{
          i => extracted(i)
        }
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualTree

  @Test
  def extractFromCase = new FileSet {
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
      private def out(a: Int): Unit = {
        /*(*/println(a)/*)*/
      }
      1 match{
        case a: Int => 
          out(a)
      }
    }
    """
  }.refactor(extract("out", "a" :: Nil)).assertEqualTree

  @Test
  def extractFromNonBlock = new FileSet {
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
  def extractFromForEnumerator = new FileSet {
    """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        for {
          i <- /*(*/1 to a/*)*/
        } yield i
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def list(a: Int) = {
        def extracted(a: Int): scala.collection.immutable.Range.Inclusive = {
          /*(*/1 to a/*)*/
        }
        for {
          i <- extracted(a)
        } yield i
      }
    }
    """
  }.refactor(extract("extracted", "a" :: Nil)).assertEqualTree

  @Test
  def extractFromYield = new FileSet {
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

  @Test
  def extractFromObjectUsingGlobalAsParameter = new FileSet {
    """
    package extractClosure
    object Demo{
      val a = 100
	  if(/*(*/a > 10/*)*/)
        println("> 10")
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      private def extracted(a: => Int): Boolean = {
        /*(*/a > 10/*)*/
      }

      val a = 100
	  if(extracted(a))
        println("> 10")
    }
    """
  }.refactor(extract("extracted", "a" :: Nil)).assertEqualCode
  
  @Test
  def extractExtractorAssignment = new FileSet {
    """
    package extractClosure
    object Demo{
      def fn = {
        /*(*/val (a, b) = (1, 2)/*)*/
        (a, b)
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def fn = {
        def extracted: (Int, Int) = {
          /*(*/val (a, b) = (1, 2)/*)*/
          (a, b)
        }
        val (a, b) = extracted
        (a, b)
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualCode
  
  @Test
  @Ignore("Depends on https://www.assembla.com/spaces/scala-refactoring/tickets/82")
  def extractLhsOfMultipleAssignment = new FileSet {
    """
    package extractClosure
    object Demo{
      def fn = {
        val (a, b) = /*(*/(1, 2)/*)*/
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def fn = {
        def extracted: (Int, Int) = {
          /*(*/(1, 2)/*)*/
        }
        val (a, b) = extracted
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualCode
  
  @Test
  @Ignore("Depends on https://www.assembla.com/spaces/scala-refactoring/tickets/82")
  def extractExpressionInParens = new FileSet {
    """
    package extractClosure
    object Demo{
      def fn = {
        (/*(*/List(1, 2, 3)/*)*/).foreach(println(_))
      }
    }
    """ becomes
      """
    package extractClosure
    object Demo{
      def fn = {
        def extracted: List[Int] = {
          /*(*/List(1, 2, 3)/*)*/
        }
        extracted.foreach(println(_))
      }
    }
    """
  }.refactor(extract("extracted", Nil)).assertEqualCode
}