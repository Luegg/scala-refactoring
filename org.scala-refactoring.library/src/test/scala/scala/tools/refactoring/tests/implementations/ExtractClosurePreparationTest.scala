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
      assertEquals(s"expected optional parameters $optional but was $opt", optional.length, opt.length)
      assertEquals(s"expected optional parameters $required but was $req", required.length, req.length)
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
  @Ignore
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
  def prepareExtractionOfMethod = Prepare(
    """
    object Demo {
      def a = {
        /*(*/def b(c: Any) = c/*)*/
      }
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareExtractionOfTypeAlias = Prepare(
    """
    object Demo {
      def a = {
        /*(*/type T = Int/*)*/
      }
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareExtractionOfClassDefinition = Prepare(
    """
    object Demo {
      def a = {
        /*(*/class A/*)*/
      }
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareExtractionOfObjectDefinition = Prepare(
    """
    object Demo {
      def a = {
        /*(*/object A/*)*/
      }
    }
    """)
    .assertFailure
    .done

  @Test
  def prepareWithClassAsInboundDependency = Prepare(
    """
    object Demo {
      def a = {
        class A
        /*(*/new A/*)*/
      }
    }
    """)
    .assertSuccess
    .assertParameters(Nil, Nil)
    .done
}