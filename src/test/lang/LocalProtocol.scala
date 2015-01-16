package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.LocalProtocol._
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import java.io.Reader

class LocalProtocol extends PathInfo {

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    val wf = new WFGlobalProtocol(g.exprs, g.x_0)
    wf
  }

  @Test def testHelloWorld() {
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))

    val localMe = LocalProjection(g, "Me").exprs.toSet
    val localWorld = LocalProjection(g, "World").exprs.toSet

    localMe foreach (f => println(f.canonical))
    println()
    localWorld foreach (f => println(f.canonical))

    assertTrue(localMe.contains(Send("x_0", "World", "Say", "Hello", "x_1")))
    assertTrue(localMe.contains(LocalProtocol.End("x_1")))

    assertTrue(localWorld.contains(Receive("x_0", "Me", "Say", "Hello", "x_1")))
    assertTrue(localWorld.contains(LocalProtocol.End("x_1")))

  }

  @Test def testOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))

    val localBuyer1 = LocalProjection(g, "Buyer1").exprs.toSet
    val localBuyer2 = LocalProjection(g, "Buyer2").exprs.toSet
    val localSeller = LocalProjection(g, "Seller").exprs.toSet

    println("##Buyer1##")
    localBuyer1 foreach (e => println(e.canonical))
    println("##Buyer2##")
    localBuyer2 foreach (e => println(e.canonical))
    println("##Seller##")
    localSeller foreach (e => println(e.canonical))

    assertTrue(localBuyer1.contains(Send("x_0", "Seller", "Book", "", "x_1")))
    assertTrue(localSeller.contains(Receive("x_0", "Buyer1", "Book", "", "x_1")))

    assertTrue(localBuyer1.contains(Receive("x_1", "Seller", "Price", "", "x_2")))
    assertTrue(localSeller.contains(Send("x_1", "Buyer1", "Price", "", "x_2")))

    assertTrue(localBuyer1.contains(Send("x_2", "Buyer2", "Quote", "", "x_3")))
    assertTrue(localBuyer2.contains(Receive("x_0", "Buyer1", "Quote", "", "x_3")))

    assertTrue(localBuyer1.contains(Receive("x_5", "Buyer2", "Quit", "", "x_6")))
    assertTrue(localBuyer2.contains(Send("x_5", "Buyer1", "Quit", "", "x_6")))

    assertTrue(localBuyer1.contains(Receive("x_11", "Buyer2", "Agree", "", "x_12")))
    assertTrue(localBuyer2.contains(Send("x_11", "Buyer1", "Agree", "", "x_12")))

    assertTrue(localBuyer1.contains(Send("x_12", "Seller", "Transfer", "", "x_13")))

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

  @Test def testLinearInteractionProjectedToA() {
    val g = getProtocol(new FR(path + "LinearInteraction.txt"))
    val local = LocalProjection(g, "A")
    
    assertTrue(local.exprs.contains(Send("x_0","L","Log","","x_1")))
    assertTrue(local.exprs.contains(Send("x_1","B","M1","","x_2")))
    assertTrue(local.exprs.contains(Send("x_2","L","Log","","x_4")))
    assertTrue(local.exprs.contains(LocalProtocol.End("x_4")))
  }
  
  @Test def testLinearInteractionProjectedToC() {
    val g = getProtocol(new FR(path + "LinearInteraction.txt"))
    val local = LocalProjection(g, "C")
    
    assertTrue(local.exprs.contains(Receive("x_0","B","M2","","x_3")))
    assertTrue(local.exprs.contains(Send("x_3","D","Log","","x_5")))
    assertTrue(local.exprs.contains(LocalProtocol.End("x_5")))
  }

}
