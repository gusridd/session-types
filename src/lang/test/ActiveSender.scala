package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class ActiveSender {
  
  def activeSenderFromString(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.checkLocalChoice()
  }

  @Test def testSimpleChoice() {
    activeSenderFromString(new StringReader("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4"))
  }
  


}
