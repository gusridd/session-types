package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader

class WFGlobalProtocolTest {

  val path = "./src/lang/test/"
  val path_wf = "./src/protocol/wellformed/"

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    val wf = new WFGlobalProtocol(g.exprs)
    wf
  }

  @Test def testLinearityHelloWorld() {
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))
  }

  @Test def testLinearityOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
  }

  @Test def testLinearityPostOffice() {
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
  }

  @Test def testLinearityTravelAgency() {
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
  }

  @Test def testInterleavedSandTSystem() {
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
  }
  
  @Test def testRecursiveChoice() {
    val g = getProtocol(new FR(path + "RecursiveChoice.txt"))
  }
  
  @Test def testInterleavedSAndTSystem() {
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
  }
}