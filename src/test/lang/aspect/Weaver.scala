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
  @Test def testLocalWeaveNonParallelizedWithGather() {
    val p = "C"
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))

    val localAspects = aspects map { a => lang.aspect.LocalProjection(a, p) }
    val localProtocol = lang.LocalProjection(protocol, p)

    val localWovenType = Congruence(Weaver.localWeaving(localAspects, localProtocol))
    println("GLOBAL ASPECT")
    println(aspects(0).toString)
    println("LOCAL PROTOCOL")
    println(localProtocol.canonical(1))
    println("LOCAL ASPECT")
    println(localAspects(0).toString)
    println("LOCAL WOVEN TYPE")
    //    localWovenType.exprs map (x => println(x.canonical))
    println(localWovenType.canonical(1))
  }

}
