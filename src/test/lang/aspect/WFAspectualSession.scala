package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr
import lang.Message
import lang.aspect.Aspect
import lang.WFGlobalProtocol
import lang.aspect.GlobalAspect
import lang.aspect.WFAspectualSession.NonWFAspectualSession

class WFAspectualSession extends PathInfo {
  import lang.aspect.WFAspectualSession

  @Test def SimpleTradeWithNegotiation() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    WFAspectualSession(aspects, protocol)
  }

  @Test def SimpleTradeWithLogging() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    WFAspectualSession(aspects, protocol)
  }

  @Test(expected = classOf[NonWFAspectualSession])
  def SimpleTradeWithAuthentication() {
    val aspects = AspectParser.parse(new FR(path_mf_a + "Authentication.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    WFAspectualSession(aspects, protocol)
  }

  @Test def NonParallelizedWithFarm() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))
    WFAspectualSession(aspects, protocol)
  }

  @Test def NonParallelizedWithGather() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))
    WFAspectualSession(aspects, protocol)
  }

  @Test  def NonParallelizedWithFarmAndGather() {
    val farm = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val gather = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val aspects = farm ++ gather
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))
    val session = WFAspectualSession(aspects, protocol)
    println("SESSION")
    session.print
  }
  
  @Test def SimpleTradeWithAuthWithLock() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "AuthWithLock.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))
    WFAspectualSession(aspects, protocol)
  }
}