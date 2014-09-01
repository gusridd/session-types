package lang

import LocalProtocol._

class WFGlobalProtocol(exprs: List[expr]) extends GlobalProtocol(exprs) {
  /**
   * Any Well-formed Global Protocol must comply with the sanity, local
   * choice and linearity conditions.
   */
  if(!Sanity(this)) throw SanityConditionException()
  if(!LocalChoice(this)) throw LocalChoiceConditionException()
  if(!Linearity(this)) throw LinearityConditionException()

  def localProjection(p: String): Iterable[localExpr] = {
    getParticipants().find(_ == p) match {
      case Some(pp) => {
        var localProtocol = lp(p)
        localProtocol foreach ( {
          case i @ LocalProtocol.Indirection(x1,x2) => {
            localProtocol = (localProtocol filterNot (_ == i)).map(_.substitute(x1, x2))
          }
          case _ =>
        })
        localProtocol
      }
      case None =>
        throw new Exception("Trying to project a non-existant participant " + p)
    }
  }

  private def lp(participant: String): Iterable[LocalProtocol.localExpr] = {
    exprs map (e => e match {
      case Message(x, p, pp, l, t, xp) if (participant == p) => Send(x, pp, l, t, xp)
      case Message(x, p, pp, l, t, xp) if (participant == pp) => Receive(x, p, l, t, xp)
      case Message(x, p, pp, l, t, xp) => LocalProtocol.Indirection(x, xp)
      case ParallelJoin(x, xp, xpp) => Join(x, xp, xpp)
      case Parallel(x, xp, xpp) => Fork(x, xp, xpp)
      case Choice(x, xp, xpp) => {
        try {
          val as = ActiveSender(this, x)
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

object WFGlobalProtocol {
  def apply(g: GlobalProtocol): WFGlobalProtocol = {
    new WFGlobalProtocol(g.exprs)
  }
}