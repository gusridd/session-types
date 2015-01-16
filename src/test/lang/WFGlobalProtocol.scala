package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader
import lang.ActiveSender.NoActiveSenders

class WFGlobalProtocolTest extends PathInfo {

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    val wf = new WFGlobalProtocol(g.exprs,g.x_0)
    wf
  }

  @Test def testHelloWorld() {
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))
  }

  @Test def testOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
  }

  @Test def testPostOffice() {
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
  }

  @Test def testTravelAgency() {
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
  }
  
  @Test def testSimpleTrade() {
    val g = getProtocol(new FR(path_wf + "SimpleTrade.txt"))
  }
  
  @Test def testTradeWithNegotiation() {
    val g = getProtocol(new FR(path_wf + "TradeWithNegotiation.txt"))
  }

  @Test def testInterleavedSandTSystem() {
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
  }
  
  @Test def testLinearInteraction() {
    val g = getProtocol(new FR(path + "LinearInteraction.txt"))
  }
  
  @Test(expected = classOf[NoActiveSenders]) 
  def testRecursiveChoice() {
    val g = getProtocol(new FR(path + "RecursiveChoice.txt"))
  }
  
  @Test def testInterleavedSAndTSystem() {
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
  }
  
  @Test(expected = classOf[lang.SanityConditionException])
  def testSPARQLv1(){
    val g = getProtocol(new FR(path + "SPARQLv1.txt"))
  }
}
