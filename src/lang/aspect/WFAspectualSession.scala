package lang.aspect

import lang.WFGlobalProtocol
import lang.expr
import lang.GlobalProtocol
import lang.End

class WFAspectualSession(aspects: List[GlobalAspect], exprs: List[expr], x_0: String) extends WFGlobalProtocol(exprs, x_0) {

  private[this] def wellFormedness(): Boolean = {
    /**
     * Every advice has an end
     */
    val ends = aspects forall {
      case GlobalAspect(name, pc, adv) => {
        val hasEnd = adv.exprs exists {
          case End(_) => true
          case _ => false
        }
        if (!hasEnd) println("[ERROR] The aspect \"" + name + "\" does not have an end")
        hasEnd
      }
    }

    /**
     * Every message from/to a daemon advice is fresh wrt G
     */

    false
  }
}

object WFAspectualSession {

  def apply(aspects: List[GlobalAspect], g: GlobalProtocol) = {
    new WFAspectualSession(aspects, g.exprs, g.x_0)
  }

}