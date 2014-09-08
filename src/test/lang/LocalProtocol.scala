package test.lang;

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
    
    val localMe = LocalProjection(g,"Me").toSet
    val localWorld = LocalProjection(g,"World").toSet
    
    assertTrue(localMe.contains(Send("x_0","World","Say","Hello","x_1")))
    assertTrue(localMe.contains(LocalProtocol.End("x_1")))
    
    assertTrue(localWorld.contains(Receive("x_0","Me","Say","Hello","x_1")))
    assertTrue(localWorld.contains(LocalProtocol.End("x_1")))
    
  }
  
  @Test def testOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
    
    val localBuyer1 = LocalProjection(g,"Buyer1").toSet
    val localBuyer2 = LocalProjection(g,"Buyer2").toSet
    val localSeller = LocalProjection(g,"Seller").toSet
    
    println("##Buyer1##")
    localBuyer1 foreach (e => println(e.canonical))
    println("##Buyer2##")
    localBuyer2 foreach (e => println(e.canonical))
    println("##Seller##")
    localSeller foreach (e => println(e.canonical))
    
    assertTrue(localBuyer1.contains(Send("x_0","Seller","Book","","x_1")))
    assertTrue(localSeller.contains(Receive("x_0","Buyer1","Book","","x_1")))
    
    assertTrue(localBuyer1.contains(Receive("x_1","Seller","Price","","x_2")))
    assertTrue(localSeller.contains(Send("x_1","Buyer1","Price","","x_2")))
    
    assertTrue(localBuyer1.contains(Send("x_2","Buyer2","Quote","","x_3")))
    assertTrue(localBuyer2.contains(Receive("x_2","Buyer1","Quote","","x_3")))
    
    assertTrue(localBuyer1.contains(Receive("x_5","Buyer2","Quit","","x_6")))
    assertTrue(localBuyer2.contains(Send("x_5","Buyer1","Quit","","x_6")))
    
    assertTrue(localBuyer1.contains(Receive("x_11","Buyer2","Agree","","x_12")))
    assertTrue(localBuyer2.contains(Send("x_11","Buyer1","Agree","","x_12")))
    
    assertTrue(localBuyer1.contains(Send("x_12","Seller","Transfer","","x_13")))
    
    
    
//    fail
  }
  
  @Test def testPostOffice() {
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
    //TODO: local projection for PostOffice
  }
  
  @Test def testTravelAgency() {
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
    //TODO: local projection for TravelAgency
  }

}
