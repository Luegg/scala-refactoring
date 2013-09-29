package scala.tools.refactoring
package tests.implementations

import tests.util
import implementations.ExtractClosure
import org.junit.Assert._

class ExtractClosureTest extends util.TestRefactoring {
  import global._

  type Params = ExtractClosure#RefactoringParameters

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
    
    new{
      def assertSuccess = 
        assertTrue(refactoringImpl.preparationResult.isRight)
      def assertFailure = 
        assertTrue(refactoringImpl.preparationResult.isLeft)
    }
  }

  @Test
  def prepareWithSelectedStatement = prepare(
    """
    package extractClosure
    object Demo {
	  val osx = "MAC"
      
	  if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
        println("you're using Mac OsX");
    }
    """
  ).assertSuccess
    
  @Test
  def prepareWithSelectedMethod = prepare(
    """
    package extractClosure
    object Demo {
      def /*(*/printString(v: Any)/*)*/ =
        println(v.toString)
    }
    """
  ).assertFailure

  @Test
  def extractClosureFromDef = new FileSet {
    """
      package extractClosure
      object Demo {
	  	val osx = "MAC"
    
	  	def printOsInfo(os: String) = {
	  	  if(/*(*/os.toUpperCase.indexOf(osx) != -1/*)*/)
            println("you're using Mac OsX");
	  	}
      }
    """ becomes
      """
      package extractLocal
      object Demo {
	  	val osx = "MAC"
    
	  	def printOsInfo(os: String) = {
    	  def isOs(osx: String) = 
            /*(*/os.toUpperCase.indexOf(osx) != -1/*)*/
    
	  	  if(isOs(osx))
            println("you're using Mac OsX");
	  	}
      }
    """
  } applyRefactoring (extract("isOs", "os" :: Nil))

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
      package extractLocal
      object Demo extends App{
        val os = System.getProperties.get("os.name")
	  	val osx = "MAC"
    
	    def isOs(osx: String) = 
          /*(*/os.toUpperCase.indexOf(osx) != -1/*)*/
    
	    if(isOs(osx))
          println("you're using Mac OsX");
      }
    """
  } applyRefactoring (extract("isOs", "os" :: Nil))
}