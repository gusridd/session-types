package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.Linearity.lin
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
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
  
  @Test def testLinearityHelloWorld(){
    val g = getProtocol(new FR(path_wf + "HelloWorld.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityOnlineBookStore(){
    val g = getProtocol(new FR(path_wf + "OnlineBookstore.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityPostOffice(){
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityTravelAgency(){
    val g = getProtocol(new FR(path_wf + "TravelAgency.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testInterleavedSandTSystem(){
    val g = getProtocol(new FR(path + "interleavedSAndTSystem.txt"))
    assertTrue(Linearity(g))
  }
}
