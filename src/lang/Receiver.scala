package lang

import scala.collection.immutable.Set

object Receiver {

  class UndefinedReceiverException(xt: List[String], pt: Set[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedReceiverException: Undefined Rcv(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def apply(g: GlobalProtocol)(x: String) = {
    new output(r(g, List(), Set(), x))
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

object Linearity {

  class UndefinedLinearityException(xt: List[String], pt: List[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedLinearityException: Undefined Lin(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def apply(g: GlobalProtocol)(x: String) = {
    new output(l(g, List(), List(), x))
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

/**
 * The output class express that the equality
 * [A]Rcv(G)(x_1) = [B]Rcv(G)(x_2) holds if forall(p:l1:xt1) in A,
 * forall(p:l2:xt2) in B, l1 != l2 or x1p and x2p share a
 * non-null suffix
 */
case class output(s: Set[(String, String, List[String])]) {
  override def equals(c: Any) = c match {
    case output(sp) => s forall (e1 => sp forall (e2 => (e1, e2) match {
      case ((p1, l1, xt1), (p2, l2, xt2)) => l1 != l2 || nonNullSuffix(xt1, xt2)
    }))
    case _ => false
  }

  final private def nonNullSuffix(xt1: List[String], xt2: List[String]): Boolean =
    (xt1, xt2) match {
      case (List(), h2 :: t2) => false
      case (h1 :: t1, List()) => false
      case (List(), List()) => false
      case _ => xt1.last == xt2.last
    }
}