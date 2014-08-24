package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.Linearity.lin
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader

class LocalChoice {

  val path = "./src/lang/test/"
  val path_wf = "./src/protocol/wellformed/"

  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  def localChoiceFromReader(reader: Reader): Boolean = {
    LocalChoice(getProtocol(reader))
  }

  @Test def testSimpleCorrectChoice() {
    assertFalse(localChoiceFromReader(new SR("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4")))
  }

  @Test def testHelloWorld() {
    assertTrue(localChoiceFromReader(new FR(path_wf + "HelloWorld.txt")))
  }
  
  @Test def testOnlineBookStore() {
    assertTrue(localChoiceFromReader(new FR(path_wf + "OnlineBookstore.txt")))
  }
  
  @Test def testPostOffice() {
    assertTrue(localChoiceFromReader(new FR(path_wf + "PostOffice.txt")))
  }
  
  @Test def testTravelAgency() {
    assertTrue(localChoiceFromReader(new FR(path_wf + "TravelAgency.txt")))
  }

  @Test def testSimpleIrrelevantCases() {
    assertTrue(localChoiceFromReader(new SR("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4")))
    assertTrue(localChoiceFromReader(new SR("x_1 = end")))
    assertTrue(localChoiceFromReader(new SR("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4 \n x_4 = end")))
  }

  //@Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoice() {
    assertFalse(localChoiceFromReader(new FR("./src/lang/test/nonLocalChoice.txt")))
  }

  //@Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoiceConfusion() {
    assertFalse(localChoiceFromReader(new FR("./src/lang/test/nonLocalChoiceConfusion.txt")))
  }
  
  @Test def testNonLocalChoiceMultiple(){
    assertFalse(localChoiceFromReader(new FR("./src/lang/test/choiceNonReceive.txt")))
  }

  /**
   * Receiver
   */
  @Test def testReceiveGreetingDecision() {
    val g = getProtocol(new FR(path + "greetingDecision.txt"))
    val s1 = Receiver(g)("x_2")
    val s2 = Receiver(g)("x_3")
    assertEquals(s1, s2)
  }

  @Test def testReceiveThreadCorrectnessGood() {
    val g = getProtocol(new FR(path + "threadCorrectnessGood1.txt"))
    val s1 = Receiver(g)("x_1")
    val s2 = Receiver(g)("x_2")
    assertEquals(s1, s2)
  }

}
