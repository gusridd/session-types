package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader

class ActiveSender extends PathInfo {

  def activeSenderFromFile(filename: String, x: String, expected: String) = {
    val reader = new FR(path + filename)
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
    val reader = new FR(path_wf+"PostOffice.txt")
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
  
  @Test def testChoiceNonReceive() {
    val reader = new FR(path + "choiceNonReceive.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals("A", ActiveSender(g,"x_0"))
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
