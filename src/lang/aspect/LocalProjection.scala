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

object LocalProjection {
  def apply(g: GlobalProtocol, p: String) = {
    g.getParticipants().find(_ == p) match {
      case Some(pp) => {
        Congruence(lp(g, p))
      }
      case None =>
        throw new Exception("Trying to project a non-existant participant " + p)
    }
  }

  private[this] def lp(g: GlobalProtocol, participant: String): Iterable[localExpr] = {
    g.exprs map (e => e match {
      case Message(x, p, pp, l, t, xp) if (participant == p) => Send(x, pp, l, t, xp)
      case Message(x, p, pp, l, t, xp) if (participant == pp) => Receive(x, p, l, t, xp)
      case Message(x, p, pp, l, t, xp) => LocalProtocol.NullAction(x, xp)
      case ParallelJoin(x, xp, xpp) => Join(x, xp, xpp)
      case Parallel(x, xp, xpp) => Fork(x, xp, xpp)
      case Choice(x, xp, xpp) => {
        try {
          val as = ActiveSender(g, x)
          if (participant == as) {
            InternalChoice(x, xp, xpp)
          } else {
            ExternalChoice(x, xp, xpp)
          }
        } catch {
          case e: Exception => ExternalChoice(x, xp, xpp)
        }
      }
      case ChoiceJoin(x, xp, xpp) => Merge(x, xp, xpp)
      case End(x) => LocalProtocol.End(x)
    })
  }
}