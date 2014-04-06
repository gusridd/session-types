package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class ActiveSender {
  
  def activeSenderFromFile(filename: String, x: String, expected :String) = {
    val reader = new FileReader(filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals(expected,ActiveSender(g,x))
  }

  @Test def testActiveSender() {
    //activeSenderFromFile("./src/lang/test/threadCorrectnessGood1.txt","x_0","Alice")
    activeSenderFromFile("./src/lang/test/greetingDecision.txt","x_1","B")
  }
  
  @Test (expected = classOf[ActiveSender.NonChoiceException])
  def testNotDefinedForNonChoice(){
    activeSenderFromFile("./src/lang/test/threadCorrectnessGood1.txt","x_1","Alice")
  }
  


}
