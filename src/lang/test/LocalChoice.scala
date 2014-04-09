package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class LocalChoice {
  
  val path = "./src/lang/test/"
  
  def localChoiceFromString(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.checkLocalChoice()
  }
  
  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  @Test def testSimpleCorrectChoice() {
    localChoiceFromString(new StringReader("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4"))
  }
  
  @Test def testSimpleIrrelevantCases() {
    localChoiceFromString(new StringReader("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4"))
    localChoiceFromString(new StringReader("x_1 = end"))
    localChoiceFromString(new StringReader("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4 \n x_4 = end"))
  }
  
  @Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoice(){
    localChoiceFromString(new FileReader("./src/lang/test/nonLocalChoice.txt"))
  }
  
  @Test(expected = classOf[lang.LocalChoiceException])
  def testNonLocalChoiceConfusion(){
    localChoiceFromString(new FileReader("./src/lang/test/nonLocalChoiceConfusion.txt"))
  }
  
  
  /**
   * Receiver
   */
  @Test def testReceiveEmpty() {
    val g = getProtocol(new FileReader(path + "greetingDecision.txt"))
    val s1 = Receiver(g)("x_2")
    val s2 = Receiver(g)("x_3")
    assertEquals(s1,s2)
    
    
  }

}
