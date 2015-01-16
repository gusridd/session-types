package lang.aspect

import lang.WFGlobalProtocol
import lang.expr
import lang.GlobalProtocol
import lang.End
import lang.aspect.WFAspectualSession.NonWFAspectualSession

class WFAspectualSession(aspects: List[GlobalAspect], exprs: List[expr], x_0: String) extends WFGlobalProtocol(exprs, x_0) {

  if(!this.wellFormedness) throw new NonWFAspectualSession(aspects)
  
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
    val gMsgLabels = getMessageLabels

    val fresh = aspects forall {
      case a @ GlobalAspect(name, pc, adv) => {
        val aDaemonMsgLabels = a.getDaemonLabels
        val fsh = (gMsgLabels & aDaemonMsgLabels).isEmpty
        if (!fsh)
          println("""[ERROR] Messages from/to the deamon advice from aspect 
              \"" + name + "\" are not fresh wrt G""")
        fsh
      }
    }

    /**
     * Aspectual linearity is satisfied
     */
    val aspLin = aspects forall { a => AspectualLinearity(this, a) }

    /**
     * Local choice condition is satisfied
     */

    val lclChoice = aspects forall { a => AspectualLocalChoice(a, this) }

    ends && fresh && aspLin && lclChoice
  }
}

object WFAspectualSession {

  def apply(aspects: List[GlobalAspect], g: GlobalProtocol) = {
    new WFAspectualSession(aspects, g.exprs, g.x_0)
  }
  
  class NonWFAspectualSession(asps: List[GlobalAspect]) extends Exception {
    override def toString(): String = {
      "Aspects: \"" + (asps map {_.name}).mkString(", ") + "\" are not well-formed"
    }
  }

}