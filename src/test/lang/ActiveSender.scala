package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import java.io.Reader

class ActiveSender extends PathInfo {

  def getASendFromFile(filename: String, x: String): String = {
    val reader = new FR(filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    ActiveSender(g, x)
  }

  @Test def testThreadCorrectnessGood1() {
    val as_x0 = getASendFromFile(path + "threadCorrectnessGood1.txt", "x_0")
    assertEquals("Alice", as_x0)
  }
  
  @Test def testNegotiationWithNoAgreement() {
    val as_x2 = getASendFromFile(path + "negotiationWithNoAgreement.txt", "x_2")
    assertEquals("Broker", as_x2)
    
    val as_x6 = getASendFromFile(path + "negotiationWithNoAgreement.txt", "x_6")
    assertEquals("Broker", as_x6)
  }
  
  @Test def testGreetingDecision() {
    val as_x1 = getASendFromFile(path + "greetingDecision.txt", "x_1")
    assertEquals("B", as_x1)
  }

  @Test def testPostOffice() {
    val reader = new FR(path_wf + "PostOffice.txt")
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
    val as = getASendFromFile(path + "choiceNonReceive.txt", "x_0")
    assertEquals("A", as)
  }

  @Test def testTradeWithNegotiation() {
    val as = getASendFromFile(path_wf + "TradeWithNegotiation.txt", "x_9")
    assertEquals("B", as)
  }

  @Test(expected = classOf[ActiveSender.NoActiveSenders])
  def testNonLocalChoice() {
    val as = getASendFromFile(path + "nonLocalChoice.txt", "x_0")
  }

  @Test(expected = classOf[ActiveSender.NoActiveSenders])
  def testRecursiveChoice() {
    val as = getASendFromFile(path + "RecursiveChoice.txt", "x_2")
  }

  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForMessage() {
    val as = getASendFromFile(path + "threadCorrectnessGood1.txt", "x_1")
  }

  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForChoiceJoin() {
    val as = getASendFromFile(path + "threadCorrectnessGood1.txt", "x_1")
  }

  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForParallel() {
    val as = getASendFromFile(path + "threadCorrectnessGood2.txt", "x_0")
  }

  @Test(expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForParallelJoin() {
    val as = getASendFromFile(path + "threadCorrectnessGood2.txt", "x_4")
  }

}
