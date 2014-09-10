package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.Linearity.lin
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import java.io.Reader

class Linearity extends PathInfo {

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  /**
   * Linearity
   */
  @Test def testLinThreadCorrectnessGood() {
    val g = getProtocol(new FR(path + "threadCorrectnessGood2.txt"))

    val s1 = lin(g)("x_1")
    val s2 = lin(g)("x_2")
    assertEquals(s1, s2)

    val s3 = lin(g)("x_5")
    val s4 = lin(g)("x_6")
    assertEquals(s3, s4)
  }

  @Test def testLinNegotiationWitNoAgreement() {
    val g = getProtocol(new FR(path + "negotiationWithNoAgreement.txt"))
    val s1 = lin(g)("x_9")
    val s2 = lin(g)("x_10")
    assertEquals(s1, s2)
  }

  @Test def testLinearityHelloWorld() {
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))
    assertTrue(Linearity(g))
  }

  @Test def testLinearityOnlineBookStore() {
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
    assertTrue(Linearity(g))
  }

  @Test def testLinearityPostOffice() {
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
    assertTrue(Linearity(g))
  }

  @Test def testLinearityTravelAgency() {
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
    assertTrue(Linearity(g))
  }

  @Test def testInterleavedSandTSystem() {
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
    assertTrue(Linearity(g))
  }

  @Test def testLinearityTradeWithLoggingAfter() {
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingAfter.txt"))

    assertFalse(Linearity(g))
  }

  @Test def testLinTradeWithLoggingAfter() {
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingAfter.txt"))
    val s1 = lin(g)("x_2")
    val s2 = lin(g)("x_3")

    assertFalse(s1.isEmpty)
    assertFalse(s2.isEmpty)
    assertFalse(s1 == s2)
  }

  @Test def testLinearityTradeWithLoggingBefore() {
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingBefore.txt"))

    assertFalse(Linearity(g))
  }

  @Test def testLinTradeWithLoggingBefore() {
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingBefore.txt"))
    val s1 = lin(g)("x_2")
    val s2 = lin(g)("x_3")

    assertFalse(s1.isEmpty)
    assertFalse(s2.isEmpty)
    assertFalse(s1 == s2)
  }

  @Test def testLinNonParallelizedWithFarm() {
    val og = getProtocol(new FR(path_wf + "WeavedNonParallelizedWithFarm.txt"))
    /**
     * The actual apply method of Linearity applies the congruence reduction
     * but for testing purposes here is needed by hand
     */
    val g = new GlobalProtocol(Congruence(og.exprs).to)
    
    val s1 = lin(g)("x_4")
    val s2 = lin(g)("x_5")

    assertFalse(s1.isEmpty)
    assertFalse(s2.isEmpty)
    assertTrue(s1 == s2)

    val s9 = lin(g)("x_9")
    val s10 = lin(g)("x_10")

    assertFalse(s9.isEmpty)
    assertFalse(s10.isEmpty)
    assertTrue(s9 == s10)

    val s11 = lin(g)("x_11")
    val s12 = lin(g)("x_12")

    assertFalse(s11.isEmpty)
    assertFalse(s12.isEmpty)
    assertTrue(s11 == s12)

    val s13 = lin(g)("x_13")
    val s14 = lin(g)("x_14")

    assertFalse(s13.isEmpty)
    assertFalse(s14.isEmpty)
    assertTrue(s13 == s14)

  }
  
  @Test def testLinearityTradeWithNegotiation() {
    val g = getProtocol(new FR(path_wf + "TradeWithNegotiation.txt"))
    
    assertTrue(Linearity(g))
  }
}
