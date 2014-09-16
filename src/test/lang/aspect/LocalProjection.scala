package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.aspect.Advice
import lang.aspect.LocalAspect
import lang.aspect.SendPC
import lang.LocalProtocol.AdviceTransition
import lang.LocalChoice
import lang.LocalProtocol.InternalChoice
import lang.LocalProtocol.Send
import lang.LocalProtocol.Receive
import lang.LocalProtocol.Merge
import lang.LocalProtocol.End
import lang.LocalProtocol.ExternalChoice

class LocalProjection extends PathInfo {
  import lang.aspect.LocalProjection

  @Test def testPreserveName() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts = LocalProjection(aspect, "S")

    assertEquals("Negotiation", Ts.name)
  }

  @Test def testPreserveProjectedParticipant() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts: LocalAspect = LocalProjection(aspect, "S")

    assertEquals("S", Ts.p)
  }

  @Test def testProjectNegotiationToS_PCCheck() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts: LocalAspect = LocalProjection(aspect, "S")
    val pc = Ts.pc

    assertTrue(pc.contains(SendPC("B", "Item", "String")))
  }

  @Test def testProjectNegotiationToS_Adv() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts: LocalAspect = LocalProjection(aspect, "S")
    val adv = Ts.adv

    assertTrue(adv.exprs.contains(AdviceTransition("x_0", "x_1")))
  }

  @Test def testProjectNegotiationToB_Adv() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts: LocalAspect = LocalProjection(aspect, "B")
    val adv = Ts.adv
    adv.exprs foreach { x => println(x.canonical) }
    
    assertTrue(adv.exprs.contains(Merge("x_1", "x_6", "x_2")))
    assertTrue(adv.exprs.contains(End("x_4")))
    assertTrue(adv.exprs.contains(AdviceTransition("x_0", "x_1")))
    assertTrue(adv.exprs.contains(Send("x_3", "C", "Offer", "Int", "x_5")))
    assertTrue(adv.exprs.contains(Receive("x_5", "C", "Counter", "Int", "x_6")))
    assertTrue(adv.exprs.contains(InternalChoice("x_2", "x_3", "x_4")))
  }
  
  @Test def testProjectNegotiationToC_Adv() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val aspect = parsed(0)

    val Ts: LocalAspect = LocalProjection(aspect, "C")
    val adv = Ts.adv
    adv.exprs foreach { x => println(x.canonical) }
    
    assertTrue(adv.exprs.contains(Merge("x_1", "x_6", "x_2")))
    assertTrue(adv.exprs.contains(End("x_4")))
    assertTrue(adv.exprs.contains(AdviceTransition("x_0", "x_1")))
    assertTrue(adv.exprs.contains(Receive("x_3", "B", "Offer", "Int", "x_5")))
    assertTrue(adv.exprs.contains(Send("x_5", "B", "Counter", "Int", "x_6")))
    assertTrue(adv.exprs.contains(ExternalChoice("x_2", "x_3", "x_4")))
  }
}