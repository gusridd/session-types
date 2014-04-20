package lang

object Linearity {

  def apply(g: GlobalProtocol): Boolean = {
    g.exprs forall (e => e match {
      case Parallel(x, xp, xpp) => lin(g)(xp) == lin(g)(xpp)
      case _ => true
    })
  }

  class UndefinedLinearityException(xt: List[String], pt: List[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedLinearityException: Undefined Lin(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def lin(g: GlobalProtocol)(x: String) = {
    new Output(l(g, List(), List(), x))
  }

  private def l(g: GlobalProtocol, xt: List[String], pt: List[String], xi: String): Set[(String, String, List[String])] = {
    val it = g.exprs.iterator
    while (it.hasNext) {
      val e = it.next
      e match {
        case ChoiceJoin(x, xp, xpp) if (xi == x || xi == xp) && xt.contains(xpp) => return Set.empty
        case End(x) if (xi == x) => return Set.empty
        case Message(x, p, pp, m, u, xp) if (xi == x) && !pt.contains(pp) => return Set((pp, m, List(xi))) ++ l(g, xt, pt, xp)
        case Choice(x, xp, xpp) if (xi == x) => return l(g, xt, pt, xp) ++ l(g, xt, pt, xpp)
        case Parallel(x, xp, xpp) if (xi == x) => return l(g, xt, pt, xp) ++ l(g, xt, pt, xpp)
        case ChoiceJoin(x, xp, xpp) if (xi == x || xi == xp) && !xt.contains(xpp) => return l(g, xt :+ xpp, pt, xpp)
        case ParallelJoin(xp, x, xpp) if (xi == x || xi == xp) => return l(g, xt, pt :+ xpp, xpp)
        case _ => return Set.empty
      }
    }
    // This will happen if the protocol is incomplete
    throw new UndefinedLinearityException(xt, pt, xi)
  }
}