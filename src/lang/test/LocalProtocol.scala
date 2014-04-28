package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.LocalProtocol._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader

class LocalProtocol {

  val path = "./src/lang/test/"
  val path_wf = "./src/protocol/wellformed/"

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    val wf = new WFGlobalProtocol(g.exprs)
    wf
  }

  @Test def testHelloWorld() {
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))
    
    val localMe = g.localProjection("Me").toSet
    val localWorld = g.localProjection("World").toSet
    
    assertTrue(localMe.contains(Send("x_0","World","Say","Hello","x_1")))
    assertTrue(localMe.contains(LocalProtocol.End("x_1")))
    
    assertTrue(localWorld.contains(Receive("x_0","Me","Say","Hello","x_1")))
    assertTrue(localWorld.contains(LocalProtocol.End("x_1")))
    
  }
  
  @Test def testOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
    fail
  }
  
  @Test def testPostOffice() {
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
    fail
  }
  
  @Test def testTravelAgency() {
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
    fail
  }




}
