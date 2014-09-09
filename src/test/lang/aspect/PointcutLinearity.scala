package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr
import lang.WFGlobalProtocol

class PointcutLinearity extends PathInfo {
  import lang.aspect.PointcutLinearity
  import lang.aspect.Weaver
  
  @Test def testPointcutLinearityAuthenticationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    
    assertFalse(PointcutLinearity(aspects,protocol))
  }
  
  @Test def testWeaveLoggingWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    protocol.print
    assertTrue(PointcutLinearity(aspects,protocol))
  }
  
  @Test def testWeaveNegotiationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    
    assertTrue(PointcutLinearity(aspects,protocol))
  }
  
  @Test def testWeaveFarmWithNonParallelized() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))
    
    assertTrue(PointcutLinearity(aspects,protocol))
  }
  
  @Test def testWeaveGatherWithNonParallelized() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))
    
    assertTrue(PointcutLinearity(aspects,protocol))
  }

}
