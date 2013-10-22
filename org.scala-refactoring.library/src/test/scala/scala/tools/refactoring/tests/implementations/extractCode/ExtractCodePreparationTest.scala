package scala.tools.refactoring
package tests.implementations.extractCode

import tests.util
import scala.tools.refactoring.implementations.extractCode.ExtractCode
import org.junit.Assert._

class ExtractCodePreparationTest extends util.TestPreparation {
  case class Prepare(fs: FileSet) extends Preparation(fs) {
    val refactoring = new ExtractCode with SilentTracing with TestProjectIndex

    import refactoring._

    def assertScopes(expectedScopes: (List[String], List[String])*) = {
      val actualScopes = preparationResult.right.get.targetScopes
      assertEquals(s"expected scopes $expectedScopes but was $actualScopes",
          expectedScopes.length, actualScopes.length)
      for(scope <- expectedScopes.zip(actualScopes)){
        assertEqualScope(scope._1, scope._2)
      }
      this
    }

    private def assertEqualScope(expected: (List[String], List[String]), actual: TargetScope) = {
      val (optExpected, reqExpected) = expected
      val TargetScope(_, optActual, reqActuel) = actual
      assertEquals(s"expected optional parameters $optExpected but was $optActual",
        optExpected.length, optActual.length)
      assertEquals(s"expected optional parameters $reqExpected but was $reqActuel",
        reqExpected.length, reqActuel.length)
      for (expectedParam <- optExpected ::: reqExpected) {
        assertTrue((optActual ::: reqActuel).exists { sym =>
          sym.nameString == expectedParam
        })
      }
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
    .assertScopes(
      (List("os", "osx"), Nil))
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
    .assertScopes(
      (List("os", "osx"), Nil),
      (List("os", "osx"), Nil))
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
    .assertScopes(
      (Nil, Nil),
      (Nil, Nil))
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
    .assertScopes(
      (Nil, List("i")),
      (Nil, List("i")))
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
    .assertScopes(
      (Nil, Nil),
      (Nil, Nil))
    .done

  @Test
  def prepareExpressionsWithListExtraction = Prepare(
    """
    package extractClosure
    object Demo {
      def a = {
        /*(*/val a :: as = List(1, 2, 3)/*)*/
	  }
    }
    """)
    .assertSuccess
    .assertScopes(
      (Nil, Nil),
      (Nil, Nil))
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
    .assertScopes((Nil, Nil))
    .done
}