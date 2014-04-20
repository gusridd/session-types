package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.Linearity.lin
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class Linearity {

  val path = "./src/lang/test/"
  val path_correct = "./src/protocol/correct/"

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  /**
   * Linearity
   */
  @Test def testLinThreadCorrectnessGood() {
    val g = getProtocol(new FileReader(path + "threadCorrectnessGood2.txt"))

    val s1 = lin(g)("x_1")
    val s2 = lin(g)("x_2")
    assertEquals(s1, s2)

    val s3 = lin(g)("x_5")
    val s4 = lin(g)("x_6")
    assertEquals(s3, s4)
  }

  @Test def testLinNegotiationWitNoAgreement() {
    val g = getProtocol(new FileReader(path + "negotiationWithNoAgreement.txt"))
    val s1 = lin(g)("x_9")
    val s2 = lin(g)("x_10")
    assertEquals(s1, s2)
  }
  
  @Test def testLinearityHelloWorld(){
    val g = getProtocol(new FileReader(path_correct + "HelloWorld.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityOnlineBookStore(){
    val g = getProtocol(new FileReader(path_correct + "OnlineBookstore.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityPostOffice(){
    val g = getProtocol(new FileReader(path_correct + "PostOffice.txt"))
    assertTrue(Linearity(g))
  }
  
  @Test def testLinearityTravelAgency(){
    val g = getProtocol(new FileReader(path_correct + "TravelAgency.txt"))
    assertTrue(Linearity(g))
  }
}
