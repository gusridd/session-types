package lang.test;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader
import java.io.StringReader

class GlobalParser {
  
  val path = "./src/lang/test/"
  
  def parseFromFile(filename : String) = {
    val reader = new FR(path + filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.getParticipants
  }
  
  def parseString(str: String) = {
    val reader = new StringReader(str)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.getParticipants
  }

  @Test def testParseSimple() {
	
    parseString("x_0 = end")
    parseString("x_0 =   end")
    parseString("x_0    = end")
    parseString("   x_0 = end")
    parseString("x_0 = end   ")

  }
  
  @Test def testParseWithError() {
	
    val p = parseString("x_0 = Me -> World : Say(Hello) x_1")
    


  }

}
