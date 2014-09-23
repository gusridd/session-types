package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr

class AspectualLocalChoice extends PathInfo {
  import lang.aspect.AspectualLocalChoice
  

  @Test def testSimpleTradeAuthentication() {
    val aspects = AspectParser.parse(new FR(path_mf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    // TODO: be sure about this
    assertTrue(AspectualLocalChoice(aspects, protocol))
  }
  
  @Test def testSimpleTradeAuthWithLock() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "AuthWithLock.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    assertTrue(AspectualLocalChoice(aspects, protocol))
  }

  @Test def testSimpleTradeLogging() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    assertTrue(AspectualLocalChoice(aspects, protocol))
  }

  @Test def testSimpleTradeNegotiation() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    assertTrue(AspectualLocalChoice(aspects, protocol))
  }
  
  @Test def testNonParalellizedFarm() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    assertTrue(AspectualLocalChoice(aspects, protocol))
  }
  
  @Test def testNonParalellizedGather() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    assertTrue(AspectualLocalChoice(aspects, protocol))
  }
}