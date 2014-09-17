package lang

import scala.collection.immutable.Set

object Receiver {

  class UndefinedReceiverException(g: GlobalProtocol, xt: List[String], pt: Set[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedReceiverException: Undefined Rcv(G," + xt + "," + pt + ")(" + xi + ")\n" + g.canonical(1)
  }

  def apply(g: GlobalProtocol)(x: String) = {
    val cg = Congruence(g)

    val (hLeft, hRight) = cg.getHashes
    hRight(x) match {
      case Choice(xch, _, _) => {
        val activeSender = ActiveSender(cg, xch)
        val setWithoutActiveSender = r(cg, xch, List(), Set(), x) filter {
          case (p, _, _) => p != activeSender
        }
        new ReceiverOutput(setWithoutActiveSender)
      }
      case _ => throw new Exception("Trying to call Rcv function on a variable that's not child of a choice")
    }

  }

  private def r(g: GlobalProtocol, xch: String, xt: List[String], pt: Set[String], xi: String): Set[(String, String, List[String])] = {
//    println("Rcv(G," + xt + "," + pt + ")(" + xi + ")")
    val (lHash, rHash) = g.getHashes
    val e = lHash(xi)
//    println(e)
    e match {
      case Message(x, p, pp, _, _, xp) if (pt contains pp) => r(g, xch, xt, pt, xp)
      case ParallelJoin(x, xpp, xp) => r(g, xch, xt, pt, xp)
      case Message(x, p, pp, l, _, xp) if !(pt contains pp) => Set((pp, l, xt)) ++ r(g, xch, xt, pt + pp, xp)
      case Choice(x, xp, xpp) if (x == xch) => Set.empty
      case Choice(x, xp, xpp) if (x != xch) => r(g, xch, xt, pt, xp) ++ r(g, xch, xt, pt, xpp)
      case Parallel(x, xp, xpp) => r(g, xch, xt, pt, xp) ++ r(g, xch, xt, pt, xpp)
      case ChoiceJoin(x, xp, xpp) if (xt contains xpp) => Set.empty
      case End(x) => Set.empty
      case ChoiceJoin(xp, x, xpp) if !(xt contains xpp) => r(g, xch, xt :+ xpp, pt, xpp)
      // This will happen if the rules are incomplete
      case _ => throw new UndefinedReceiverException(g, xt, pt, xi)
    }
  }
}

