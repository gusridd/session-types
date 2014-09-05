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
  
  @Test def testNaiveWeaveAuthenticationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol.exprs)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testNaiveWeaveLoggingWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol.exprs)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testNaiveWeaveNegotiationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    println(aspects)
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    println(protocol)
    val wovenType = Weaver.naiveGlobalWeaving(aspects, protocol.exprs)
    println(wovenType)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testWeaveAuthenticationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol.exprs)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testWeaveLoggingWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    val wovenType = Weaver.GlobalWeaving(aspects, protocol.exprs)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testWeaveNegotiationWithSimpleTrade() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    println(aspects)
    val protocol = GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt"))
    println(protocol)
    val wovenType = Weaver.GlobalWeaving(aspects, protocol.exprs)
    println(wovenType)
    wovenType map (x => println(x.canonical))
  }
  
  @Test def testLabels(){
    
  }

}
