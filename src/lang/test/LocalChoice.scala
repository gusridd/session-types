package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class LocalChoice {

  val path = "./src/lang/test/"
  val path_correct = "./src/protocol/correct/"

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  def localChoiceFromReader(reader: Reader): Boolean = {
    LocalChoice(getProtocol(reader))
  }

  @Test def testSimpleCorrectChoice() {
    assertFalse(localChoiceFromReader(new StringReader("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4")))
  }

  @Test def testHelloWorld() {
    assertTrue(localChoiceFromReader(new FileReader(path_correct + "HelloWorld.txt")))
  }
  
  @Test def testOnlineBookStore() {
    assertTrue(localChoiceFromReader(new FileReader(path_correct + "OnlineBookstore.txt")))
  }
  
  @Test def testPostOffice() {
    assertTrue(localChoiceFromReader(new FileReader(path_correct + "PostOffice.txt")))
  }
  
  @Test def testTravelAgency() {
    assertTrue(localChoiceFromReader(new FileReader(path_correct + "TravelAgency.txt")))
  }

  @Test def testSimpleIrrelevantCases() {
    assertTrue(localChoiceFromReader(new StringReader("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4")))
    assertTrue(localChoiceFromReader(new StringReader("x_1 = end")))
    assertTrue(localChoiceFromReader(new StringReader("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4 \n x_4 = end")))
  }

  //@Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoice() {
    assertFalse(localChoiceFromReader(new FileReader("./src/lang/test/nonLocalChoice.txt")))
  }

  //@Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoiceConfusion() {
    assertFalse(localChoiceFromReader(new FileReader("./src/lang/test/nonLocalChoiceConfusion.txt")))
  }

  /**
   * Receiver
   */
  @Test def testReceiveGreetingDecision() {
    val g = getProtocol(new FileReader(path + "greetingDecision.txt"))
    val s1 = Receiver(g)("x_2")
    val s2 = Receiver(g)("x_3")
    assertEquals(s1, s2)
  }

  @Test def testReceiveThreadCorrectnessGood() {
    val g = getProtocol(new FileReader(path + "threadCorrectnessGood1.txt"))
    val s1 = Receiver(g)("x_1")
    val s2 = Receiver(g)("x_2")
    assertEquals(s1, s2)
  }

  /**
   * Linearity
   */
  @Test def testLinearityThreadCorrectnessGood() {
    val g = getProtocol(new FileReader(path + "threadCorrectnessGood2.txt"))

    val s1 = Linearity(g)("x_1")
    val s2 = Linearity(g)("x_2")
    assertEquals(s1, s2)

    val s3 = Linearity(g)("x_5")
    val s4 = Linearity(g)("x_6")
    assertEquals(s3, s4)
  }

  @Test def testLinearityNegotiationWitNoAgreement() {
    val g = getProtocol(new FileReader(path + "negotiationWithNoAgreement.txt"))
    val s1 = Linearity(g)("x_9")
    val s2 = Linearity(g)("x_10")
    assertEquals(s1, s2)
  }
}
