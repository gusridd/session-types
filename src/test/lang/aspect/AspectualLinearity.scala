package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser

class AspectualLinearity extends PathInfo{
	import lang.aspect.AspectualLinearity

  /**
   * This one is tricky as in the paper says it should pass, but it shall not
   */
  @Test def testSimpleTradeAuthentication() {
    val aspects = AspectParser.parse(new FR(path_mf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    
    assertTrue(AspectualLinearity(protocol,aspects(0)))
  }

  @Test def testSimpleTradeLogging() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    
    assertTrue(AspectualLinearity(protocol,aspects(0)))
  }

  @Test def testSimpleTradeNegotiation() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    assertTrue(AspectualLinearity(protocol,aspects(0)))
  }

  @Test def testNonParalellizedFarm() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    assertTrue(AspectualLinearity(protocol,aspects(0)))
  }

  @Test def testNonParalellizedGather() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    assertTrue(AspectualLinearity(protocol,aspects(0)))
  }
  
}