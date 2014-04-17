package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class ActiveSender {

  val path = "./src/lang/test/"

  def activeSenderFromFile(filename: String, x: String, expected: String) = {
    val reader = new FileReader(path + filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals(expected, ActiveSender(g, x))
  }

  @Test def testActiveSender() {
    activeSenderFromFile("threadCorrectnessGood1.txt", "x_0", "Alice")
    activeSenderFromFile("greetingDecision.txt", "x_1", "B")
    
    activeSenderFromFile("negotiationWithNoAgreement.txt", "x_2", "Broker")
    activeSenderFromFile("negotiationWithNoAgreement.txt", "x_6", "Broker")
  }
  
  @Test def testPostOffice() {
    val reader = new FileReader("./src/protocol/correct/PostOffice.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals("S", ActiveSender(g, "x_8"))
    assertEquals("C", ActiveSender(g, "x_10"))
    assertEquals("S", ActiveSender(g, "x_12"))
    assertEquals("C", ActiveSender(g, "x_22"))
    assertEquals("C", ActiveSender(g, "x_24"))
    assertEquals("S", ActiveSender(g, "x_27"))
  }
  
  @Test def testPostOffice2() {
    val reader = new FileReader("./src/protocol/correct/PostOffice2.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals("C", ActiveSender(g, "x_5"))
    assertEquals("S", ActiveSender(g, "x_7"))
    assertEquals("C", ActiveSender(g, "x_14"))
    assertEquals("S", ActiveSender(g, "x_20"))
    assertEquals("C", ActiveSender(g, "x_23"))
    assertEquals("C", ActiveSender(g, "x_27"))
    assertEquals("S", ActiveSender(g, "x_33"))
  }
  
  
  @Test(expected = classOf[ActiveSender.NoActiveSenders])
  def testNonLocalChoice() {
    activeSenderFromFile("nonLocalChoice.txt", "x_0", "Any")
  }

  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForMessage() {
    activeSenderFromFile("threadCorrectnessGood1.txt", "x_1", "Any")
  }
  
  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForChoiceJoin() {
    activeSenderFromFile("threadCorrectnessGood1.txt", "x_1", "Any")
  }
  
  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForParallel() {
    activeSenderFromFile("threadCorrectnessGood2.txt", "x_0", "Any")
  }
  
  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForParallelJoin() {
    activeSenderFromFile("threadCorrectnessGood2.txt", "x_4", "Any")
  }

}
