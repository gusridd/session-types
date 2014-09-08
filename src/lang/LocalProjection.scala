package lang

import lang.LocalProtocol.Send
import lang.LocalProtocol.Receive
import lang.LocalProtocol.Send
import lang.LocalProtocol.Join
import lang.LocalProtocol.Fork
import lang.LocalProtocol.InternalChoice
import lang.LocalProtocol.ExternalChoice
import lang.LocalProtocol.Merge
import lang.LocalProtocol.NullAction
import LocalProtocol.localExpr

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
      case Message(x, p, pp, l, t, xp) => NullAction(x, xp)
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