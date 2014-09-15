package lang.aspect

import lang.GlobalProtocol
import lang.Message
import lang.expr

/**
 * This condition is true, if for all M such that match(pc,M), adv is
 * single-threaded wrt M
 */
object SgTh {

  def apply(g: GlobalProtocol, a: Aspect): Boolean = {
    val matches = g.exprs filter { e => Weaver.pointcutMatchGlobal(a.pc, e) }

    matches forall {
      case m @ Message(x1, p, pp, l, u, x2) => {
        val Ta_p = LocalProjection(a,g,p)
        val Ta_pp = LocalProjection(a,g,pp)
        false
      }
    }
    false
  }

  //  private[this] def singleThread(m: Message): Boolean = {
  //    val bs = Set(false, true)
  //    m match {
  //      case Message(x1,p,pp,l,u,x2) => bs exist b => STh()
  //    }
  //    false
  //  }

  private[this] def STh(e: expr, xb: List[String], b: Boolean): Boolean = {
    false
  }

}