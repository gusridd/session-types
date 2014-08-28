package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr


class Weaver extends PathInfo {
  import lang.aspect.Weaver
  
  @Test def testParseAuthentication() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol.exprs)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testParseLogging() {
    val aspect = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
  }
  
  @Test def testParseNegotiation() {
    val aspect = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
  }

}
