package scala.tools.refactoring.tests
package common

import scala.tools.refactoring.common.Occurrences
import scala.tools.refactoring.MultiStageRefactoring
import org.junit.Assert._
import org.junit.Test
import scala.reflect.internal.util.Position

class OccurrencesTest extends util.TestRefactoring {
  implicit val toOccurrence = Occurrences.posToBeginLengthPair

  def initRefactoring(code: String) = {
    val fs = new FileSet {
      code becomes ""
    }
    val r = new TestRefactoringImpl(fs) {
      val refactoring = new MultiStageRefactoring with SilentTracing with TestProjectIndex with Occurrences[(Int, Int)] {
        def prepare(selection: Selection) = ???
        def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters) = ???
        val s = selection(this, fs)
      }
    }
    r.refactoring
  }

  @Test
  def termNameOccurrences = {
    val r = initRefactoring("""
    object Demo{
      def fn = {
        /*(*/val a = 1
        val aa = 2
        val aaa = a * aa * aa/*)*/
      }
    }
    """)

    val (o1, o2) = global.ask { () =>
      (r.termNameOccurrences(r.s, "a"), r.termNameOccurrences(r.s, "aa"))
    }
    assertEquals(
      List((52, 1), (95, 1)),
      o1)

    assertEquals(
      List((70, 2), (99, 2), (104, 2)),
      o2)
  }

  @Test
  def defNameOccurrencesInTemplate = {
    val r = initRefactoring("""
    object Demo{
      /*(*/def a = 1/*)*/
      def aa = a * 2 * a
    }
    """)

    val o1 = global.ask { () =>
      r.termNameOccurrences(r.s, "a")
    }
    
    assertEquals(
      List((33, 1), (59, 1), (67, 1)),
      o1)
  }

  @Test
  def paramOccurrences = {
    val r = initRefactoring("""
    object Demo{
      def list(a: Int) = {
        /*(*/def extracted(ex: Int): Int = {
          ex * a * ex
        }
        for(ex <- 1 to 10) yield extracted(ex)/*)*/
      }
    }
    """)
    assertEquals(
      List(List((72, 2), (100, 2), (109, 2))),
      r.defDefParameterOccurrences(r.s, "extracted"))
  }

  @Test
  def multiParamOccurrences = {
    val r = initRefactoring("""
    object Demo{
      def list(a: Int) = {
        /*(*/def extracted(a: Int, bc: Int, de: Int): Int = {
          a * bc * de * bc * a
        }
        for(bc <- 1 to 10; de <- 1 to 10) yield extracted(a, bc, de)/*)*/
      }
    }
    """)
    assertEquals(
      List(
        List((72, 1), (117, 1), (136, 1)),
        List((80, 2), (121, 2), (131, 2)),
        List((89, 2), (126, 2))),
      r.defDefParameterOccurrences(r.s, "extracted"))
  }
}