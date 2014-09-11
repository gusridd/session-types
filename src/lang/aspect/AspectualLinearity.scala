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
    PointcutLinearity(a, wf) && AdviceLinearity(wf, a)
  }

}