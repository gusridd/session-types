package test.lang;

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import java.io.Reader
import java.io.StringReader

class GlobalParser extends PathInfo {

  def parseFromFile(filename: String) = {
    val reader = new FR(filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  def parseString(str: String) = {
    val reader = new StringReader(str)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }

  @Test def testParseSimple() {

    parseString("x_0 = end")
    parseString("x_0 =   end")
    parseString("x_0    = end")
    parseString("   x_0 = end")
    parseString("x_0 = end   ")

    parseString("x_0 = Me -> World : Say(Hello); x_1")
    parseString("  x_0 = Me -> World : Say(Hello); x_1")
    parseString("x_0   = Me -> World : Say(Hello); x_1")
    parseString("x_0 =   Me -> World : Say(Hello); x_1")
    parseString("x_0 = Me   -> World : Say(Hello); x_1")
    parseString("x_0 = Me ->   World : Say(Hello); x_1")
    parseString("x_0 = Me -> World   : Say(Hello); x_1")
    parseString("x_0 = Me -> World :   Say(Hello); x_1")
    parseString("x_0 = Me -> World : Say(  Hello); x_1")
    parseString("x_0 = Me -> World : Say(Hello  ); x_1")
    parseString("x_0 = Me -> World : Say(Hello)  ; x_1")
    parseString("x_0 = Me -> World : Say(Hello);   x_1")
    parseString("x_0 = Me -> World : Say(Hello); x_1  ")

  }
  @Test(expected = classOf[Exception])
  def testParseWithError() {

    parseString("x_0 = endo")
    parseString("x_0  end")
    parseString("x_ = end")
    parseString("y_0 = end")
    parseString("x0 = end")
    parseString("x_0 = ind")

    parseString("x_0 = Me -> World : Say(Hello) x_1")
    parseString("x0 = Me -> World : Say(Hello) x_1")
    parseString("x_0 = Me -> World : Say(Hello) x1")
    parseString("x_0 = me -> World : Say(Hello) x_1")
    parseString("x_0 = Me -> world : Say(Hello) x_1")
    parseString("x_0 = Me -> World : say(Hello) x_1")
    parseString("x_0 = Me -> World : Say(hello) x_1")

  }

  @Test def testDifferent_x_0() {
    val g = parseFromFile(path + "Different_x_0.txt")
    assertEquals("x_10", g.x_0)
  }

}
