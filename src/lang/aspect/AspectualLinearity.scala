package lang.aspect

import lang.GlobalProtocol
import lang.WFGlobalProtocol

/**
 * An aspect A satisfies the aspectual linearity condition wrt a base type G
 * iff A is either pointcut-linear or advice-linear wft G.
 */
object AspectualLinearity {

  def apply(g: GlobalProtocol, a: Aspect) = {
    val wf = WFGlobalProtocol(g)
    val pcLinearity = PointcutLinearity(a, wf)
    val advLinearity = AdviceLinearity(wf, a)
    val r = pcLinearity || advLinearity
    if (r) {
      println("[SUCCESS] AspectualLinearity")
    } else {
      println("[FAIL] AspectualLinearity")
      println("     ] pcLinearity: " + pcLinearity)
      println("     ] advLinearity: " + advLinearity)
    }
    r
  }

}