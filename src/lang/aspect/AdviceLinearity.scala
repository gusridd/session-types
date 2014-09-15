package lang.aspect

import lang.GlobalProtocol
import lang.WFGlobalProtocol

/**
 * An advice A is advice-linear wft a global type G iff G,A comply with the
 * uniqueMsg and for all M such that match(pc,M), adv is single-threaded wrt M.
 */
object AdviceLinearity {

  def apply(g: GlobalProtocol, a: Aspect): Boolean = {
    val uniMsg = uniqueMsg(g, a)
    val sgTh = SgTh(g, a)
    val r = uniMsg && sgTh
    if (r) {
      println("[SUCCESS] AdviceLinearity")
    } else {
      println("[FAIL] AdviceLinearity")
      println("     ] uniMsg: " + uniMsg)
      println("     ] sgTh: " + sgTh)
    }
    r

  }

}