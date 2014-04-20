package lang

import scala.collection.immutable.Set

object Receiver {

  class UndefinedReceiverException(xt: List[String], pt: Set[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedReceiverException: Undefined Rcv(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def apply(g: GlobalProtocol)(x: String) = {
    new Output(r(g, List(), Set(), x))
  }

  private def r(g: GlobalProtocol, xt: List[String], pt: Set[String], xi: String): Set[(String, String, List[String])] = {
    val it = g.exprs.iterator
    while (it.hasNext) {
      val e = it.next
      e match {
        case Message(x, p, pp, _, _, xp) if (x == xi && (pt contains pp)) => return r(g, xt, pt, xp)
        case ParallelJoin(x, xpp, xp) if (x == xi || xpp == xi) => return r(g, xt, pt, xp)
        case Message(x, p, pp, l, _, xp) if (x == xi && !(pt contains pp)) => return Set((pp, l, xt)) ++ r(g, xt, pt + pp, xp)
        case Choice(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)
        case Parallel(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)
        case ChoiceJoin(x, xp, xpp) if ((x == xi || xp == xi) && (xt contains xpp)) => return Set.empty
        case End(x) if (x == xi) => return Set.empty
        case ChoiceJoin(xp, x, xpp) if (x == xi || xp == xi) && !(xt contains xpp) => return r(g, xt :+ xpp, pt, xpp)
        case _ => return Set.empty
      }
    }
    // This will happen if the protocol is incomplete
    throw new UndefinedReceiverException(xt, pt, xi)
  }
}

