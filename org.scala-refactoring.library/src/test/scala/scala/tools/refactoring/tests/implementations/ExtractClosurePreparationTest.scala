package scala.tools.refactoring
package tests.implementations

import tests.util
import implementations.ExtractClosure
import org.junit.Assert._

class ExtractClosurePreparationTest extends util.TestPreparation {
  case class Prepare(fs: FileSet) extends Preparation(fs) {
    val refactoring = new ExtractClosure with SilentTracing with TestProjectIndex

    val PreparationResult = refactoring.PreparationResult

    def assertParameters(optional: List[String], required: List[String]) = {
      val Right(PreparationResult(_, opt, req)) = preparationResult
      assertEquals(optional.length, opt.length)
      assertEquals(required.length, req.length)
      for (expected <- optional ::: required) {
        assertTrue((opt ::: req).exists { sym =>
          sym.nameString == expected
        })
      }
      this
    }
  }

  @Test
  def prepareExpressionInObjectNotDefiningNewMembers = Prepare(
    """
    package extractClosure
    object Demo {
      val os = "abc"
      val osx = "MAC"
      
      if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
        println("you're using Mac OsX");
    }
    """)
    .assertSuccess
    .assertParameters(List("os", "osx"), Nil)
    .done

  @Test
  def prepareExpressionsDefiningNewMembers = Prepare(
    """
    package extractClosure
    object Demo {
      /*(*/val a = 1/*)*/
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareExpressionInMethod = Prepare(
    """
    package extractClosure
    object Demo {
      val os = "abc"
      val osx = "MAC"
      
      def printOsInfo =
        if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
          println("you're using Mac OsX");
    }
    """)
    .assertSuccess
    .assertParameters(List("os", "osx"), Nil)
    .done

  @Test
  def prepareSeqOfExpressions = Prepare(
    """
    package extractClosure
    object Demo {
      def printThree = {
        /*(*/val a = 1
        val b = 2/*)*/
        println(a+b)
      }
    }
    """)
    .assertSuccess
    .assertParameters(Nil, Nil)
    .done

  @Test
  def prepareWithInvalidSelection = Prepare(
    """
    package extractClosure
    object Demo {
      /*(*/def printString/*)*/(v: Any) =
      println(v.toString)
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareWithInevitableParameter = Prepare(
    """
    package extractClosure
    object Demo {
      def a = {
        ((i: Int) => /*(*/println(i)/*)*/)(1)
	  }
    }
    """)
    .assertSuccess
    .assertParameters(Nil, List("i"))
    .done

  @Test
  def prepareExpressionsWithTuppleExtraction = Prepare(
    """
    package extractClosure
    object Demo {
      def a = {
        /*(*/val (a, b) = (1, 2)
	    a * b/*)*/
	  }
    }
    """)
    .assertSuccess
    .assertParameters(Nil, Nil)
    .done

  @Test
  def prepareExtractionOfClosure = Prepare(
    """
    object Demo {
      def a = {
        println("first")
        /*(*/def b(c: Any) = c/*)*/
        println(b("second"))
      }
    }
    """)
    .assertFailure
    .done
}