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
