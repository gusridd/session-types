package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr
import lang.Congruence
import lang.LocalProtocol.Send
import lang.LocalProtocol.SimpSend
import lang.LocalProtocol.SimpReceive

class Weaver extends PathInfo {
  import lang.aspect.Weaver

  @Test def testNaiveWeaveAuthenticationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testNaiveWeaveLoggingWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testNaiveWeaveNegotiationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testWeaveAuthenticationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testWeaveLoggingWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testWeaveNegotiationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testWeaveNonParallelizedWithFarm() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  @Test def testWeaveNonParallelizedWithGather() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol)
    wovenType.exprs map (x => println(x.canonical))
  }

  /**
   * Local Weaving
   */
  @Test def testLocalWeaveSimpleTradeWithNegotiationParticipantC() {
    val p = "C"

    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpSend("B", "Counter", "Int")))
    assertTrue(localWovenType.contains(SimpReceive("B", "Offer", "Int")))
    assertTrue(localWovenType.contains(SimpReceive("B", "Purchase", "Boolean")))
  }

  @Test def testLocalWeaveSimpleTradeWithNegotiationParticipantB() {
    val p = "B"

    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpReceive("C", "Counter", "Int")))
    assertTrue(localWovenType.contains(SimpSend("C", "Offer", "Int")))
    assertTrue(localWovenType.contains(SimpSend("C", "Purchase", "Boolean")))
    assertTrue(localWovenType.contains(SimpReceive("S", "Item", "String")))
    assertTrue(localWovenType.contains(SimpSend("S", "Sale", "Boolean")))
  }

  @Test def testLocalWeaveSimpleTradeWithNegotiationParticipantS() {
    val p = "S"

    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpSend("B", "Item", "String")))
    assertTrue(localWovenType.contains(SimpReceive("B", "Sale", "Boolean")))
  }

  @Test def testLocalWeaveSimpleTradeWithLoggingParticipantB() {
    val p = "B"

    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpReceive("S", "Item", "String")))
    assertTrue(localWovenType.contains(SimpSend("S", "Sale", "Boolean")))
    assertTrue(localWovenType.contains(SimpSend("C", "Purchase", "Boolean")))
    assertTrue(localWovenType.contains(SimpSend("L", "LogData", "String")))

  }
  
  @Test def testLocalWeaveSimpleTradeWithAuthenticationParticipantB() {
    val p = "B"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol)) 
    
    assertTrue(localWovenType.contains(SimpReceive("A", "Retry", "")))
    assertTrue(localWovenType.contains(SimpSend("A", "Auth", "String")))
    assertTrue(localWovenType.contains(SimpSend("S", "Sale", "Boolean")))
    assertTrue(localWovenType.contains(SimpReceive("S", "Item", "String")))
    assertTrue(localWovenType.contains(SimpSend("C", "Purchase", "Boolean")))
    assertTrue(localWovenType.contains(SimpReceive("A", "OK", "")))
  }

  @Test def testLocalWeaveSimpleTradeWithAuthenticationParticipantC() {
    val p = "C"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpReceive("B", "Purchase", "Boolean")))
  }
  
  @Test def testLocalWeaveSimpleTradeWithAuthenticationParticipantA() {
    val p = "A"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpSend("B", "Retry", "")))
    assertTrue(localWovenType.contains(SimpReceive("B", "Auth", "String")))
    assertTrue(localWovenType.contains(SimpSend("B", "OK", "")))
  }
  
  

  @Test def testLocalWeaveNonTarallelizedWithFarmParticipantS() {
    val p = "S"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpSend("W", "Send", "")))
    assertTrue(localWovenType.contains(SimpSend("W1", "Send", "")))
    assertTrue(localWovenType.contains(SimpSend("W2", "Send", "")))
    assertTrue(localWovenType.contains(SimpSend("W3", "Send", "")))
    assertTrue(localWovenType.contains(SimpSend("W4", "Send", "")))
  }
  
  @Test def testLocalWeaveNonTarallelizedWithFarmParticipantW() {
    val p = "W"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpReceive("S", "Send", "")))
    assertTrue(localWovenType.contains(SimpSend("T", "Result", "")))
  }
  
  @Test def testLocalWeaveNonTarallelizedWithFarmParticipantT() {
    val p = "T"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))

    assertTrue(localWovenType.contains(SimpReceive("W", "Result", "")))
  }

  @Test def testLocalWeaveNonTarallelizedWithGatherParticipantW() {
    val p = "W"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "NonParallelized.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))
    //    println("GLOBAL ASPECT")
    //    println(aspects(0).toString)
    //    println("LOCAL PROTOCOL")
    //    println(localProtocol.canonical(1))
    //    println("LOCAL ASPECT")
    //    println(localAspects(0).toString)
    //    println("LOCAL WOVEN TYPE")
    //    println(localWovenType.canonical(1))

    assertTrue(localWovenType.contains(SimpSend("T", "Result", "")))
    assertTrue(localWovenType.contains(SimpSend("W1", "Ready", "")))
    assertTrue(localWovenType.contains(SimpSend("W2", "Ready", "")))
    assertTrue(localWovenType.contains(SimpSend("W3", "Ready", "")))
    assertTrue(localWovenType.contains(SimpSend("W4", "Ready", "")))
  }

}
