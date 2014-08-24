package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import lang.Linearity.lin
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader
import scala.collection.immutable.Set

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
    assertFalse(localChoiceFromReader(new SR("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4 \n x_4 = end")))
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
  @Test def testReceiveGreetingDecisionEquality() {
    val g = getProtocol(new FR(path + "greetingDecision.txt"))
    val s1 = Receiver(g)("x_2")
    val s2 = Receiver(g)("x_3")
    
    assertEquals(s1, s2)
  }

  @Test def testReceiveThreadCorrectnessGoodEquality() {
    val g = getProtocol(new FR(path + "threadCorrectnessGood1.txt"))
    val s1 = Receiver(g)("x_1")
    val s2 = Receiver(g)("x_2")
    assertEquals(s1, s2)
  }
  
  @Test def testReceiveChoiceNonReceiveOutput(){
    val g = getProtocol(new FR(path + "choiceNonReceive.txt"))
    val s1 = Receiver(g)("x_1")
    val s2 = Receiver(g)("x_3")

    // This is to ensure that the Receive is actually working
    assertEquals(Set(("B","Hi",List())),s1.s)
    assertEquals(Set(("C","Hello",List())),s2.s)
  }
  
  @Test def testReceiveChoiceNonReceiveEquality(){
    val g = getProtocol(new FR(path + "choiceNonReceive.txt"))
    val s1 = Receiver(g)("x_1")
    val s2 = Receiver(g)("x_2") 
    assertEquals(s1, s2)
  }
  
  @Test def testPostOfficeEquality(){
    val g = getProtocol(new FR(path_wf + "PostOffice.txt"))
    val s9 = Receiver(g)("x_9")
    val s6 = Receiver(g)("x_6") 
    assertEquals(s9, s6)
    
    val s13 = Receiver(g)("x_13")
    val s8 = Receiver(g)("x_8") 
    assertEquals(s13, s8)
    
    val s15 = Receiver(g)("x_15")
    val s19 = Receiver(g)("x_19") 
    assertEquals(s15, s19)
    
    val s21 = Receiver(g)("x_21")
    val s39 = Receiver(g)("x_39") 
    assertEquals(s21, s39)
    
    val s24 = Receiver(g)("x_24")
    val s27 = Receiver(g)("x_27") 
    assertEquals(s24, s27)
    
    val s28 = Receiver(g)("x_28")
    val s32 = Receiver(g)("x_32") 
    assertEquals(s28, s32)
    
    val s34 = Receiver(g)("x_34")
    val s37 = Receiver(g)("x_37") 
    assertEquals(s34, s37)
  }
  
  

}
