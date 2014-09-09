package lang

object Linearity {

  def apply(g: GlobalProtocol): Boolean = {
    g.exprs forall (e => e match {
      case Parallel(x, xp, xpp) => lin(g)(xp) == lin(g)(xpp)
      case _ => true
    })
  }

  class UndefinedLinearityException(xt: List[String], xtt: List[String], pt: List[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedLinearityException: Undefined Lin(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def lin(g: GlobalProtocol)(x: String) = {
    new ReceiverOutput(l(g, List(), List(), List(), x))
  }

  private def l(g: GlobalProtocol, xm: List[String], xj: List[String], pt: List[String], xi: String): Set[(String, String, List[String])] = {
    val (lHash, rHash) = g.getHashes
    val e = lHash(xi)
    println("Lin(G," + xm + ", " + pt + ")(" + xi + ")")
    println(e)
    e match {
      case ChoiceJoin(x, xp, xpp) if (xm.contains(xpp)) => Set()
      case End(x) => Set()
      case Message(x, p, pp, m, u, xp) if (!pt.contains(pp)) => Set((p + pp, m, xj)) ++ l(g, xm, xj, pt :+ pp, xp)
      case Message(x, p, pp, m, u, xp) => l(g, xm, xj, pt, xp)
      case Choice(x, xp, xpp) => l(g, xm, xj, pt, xp) ++ l(g, xm, xj, pt, xpp)
      case Parallel(x, xp, xpp) => l(g, xm, xj, pt, xp) ++ l(g, xm, xj, pt, xpp)
      case ChoiceJoin(x, xp, xpp) if (!xm.contains(xpp)) => l(g, xm :+ xpp, xj, pt, xpp)
      case ParallelJoin(x,xp,xpp) => l(g,xm,xj :+ xpp,pt,xpp)
      case _ => throw new UndefinedLinearityException(xm, xj, pt, xi)
    }

  }
}