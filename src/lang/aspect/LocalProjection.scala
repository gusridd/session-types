package lang.aspect

import lang.ActiveSender
import lang.Choice
import lang.ChoiceJoin
import lang.Congruence
import lang.End
import lang.GlobalProtocol
import lang.LocalProtocol
import lang.LocalProtocol.ExternalChoice
import lang.LocalProtocol.Fork
import lang.LocalProtocol.InternalChoice
import lang.LocalProtocol.Join
import lang.LocalProtocol.Merge
import lang.LocalProtocol.Receive
import lang.LocalProtocol.Send
import lang.LocalProtocol.localExpr
import lang.Message
import lang.Parallel
import lang.ParallelJoin
import lang.expr

object LocalProjection {

  def apply(a: GlobalAspect, p: String): LocalAspect = {
    new LocalAspect(
      a.name,
      p, Congruence(pcLocalProjection(a.pc, p)),
      Congruence(advLocalProjection(a.adv, p)))
  }

  private[this] def lp(a: GlobalAdvice, participant: String): List[localExpr] = {
    val exprs = a.exprs
    exprs map (e => e match {
      case Message(x, p, pp, l, t, xp) if (participant == p) => Send(x, pp, l, t, xp)
      case Message(x, p, pp, l, t, xp) if (participant == pp) => Receive(x, p, l, t, xp)
      case Message(x, p, pp, l, t, xp) => LocalProtocol.NullAction(x, xp)
      case ParallelJoin(x, xp, xpp) => Join(x, xp, xpp)
      case Parallel(x, xp, xpp) => Fork(x, xp, xpp)
      case Choice(x, xp, xpp) => {
        try {
          // This is a trick in order to get the active sender efficiently
          val g = new GlobalProtocol(exprs,a.xa)
          val as = ActiveSender(g, x)
          if (participant == as) {
            InternalChoice(x, xp, xpp)
          } else {
            ExternalChoice(x, xp, xpp)
          }
        } catch {
          case e: Exception => {
            println("[EXCEPTION] " + e.toString())
            ExternalChoice(x, xp, xpp)
          }
        }
      }
      case AdviceTransition(x1, x2) => LocalProtocol.AdviceTransition(x1, x2)
      case ChoiceJoin(x, xp, xpp) => Merge(x, xp, xpp)
      case End(x) => LocalProtocol.End(x)
    })
  }

  private[this] def pcLocalProjection(pc: GlobalPointcut, p: String): LocalPointcut = {
    LocalPointcut(pc.pcs map ({
      case GlobalPC(s, r, l, u) if (p == s) => SendPC(r, l, u)
      case GlobalPC(s, r, l, u) if (p == r) => ReceivePC(s, l, u)
      case GlobalPC(s, r, l, u) => NullPC()
    }))
  }

  private[this] def advLocalProjection(adv: GlobalAdvice, p: String): LocalAdvice = {
    new LocalAdvice(lp(adv, p), adv.xa)
  }
}