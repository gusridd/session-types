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
    
    val localBuyer1 = g.localProjection("Buyer1").toSet
    val localBuyer2 = g.localProjection("Buyer2").toSet
    val localSeller = g.localProjection("Seller").toSet
    
    assertTrue(localBuyer1.contains(Send("x_0","Seller","Book","","x_1")))
    assertTrue(localBuyer1.contains(Receive("x_1","Seller","Price","","x_2")))
    assertTrue(localBuyer1.contains(Send("x_2","Buyer2","Quote","","x_3")))
    
    localBuyer1 foreach (e => println(e))
    
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
