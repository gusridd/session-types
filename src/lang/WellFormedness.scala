package lang

import scala.collection.immutable.Set
import scala.collection.mutable.HashMap

object WellFormedness {

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

  object LocalChoice {
    def apply(g: GlobalProtocol): Boolean = {
      try {
        g.exprs forall (e => e match {
          case Choice(x, xp, xpp) => Receiver(g)(xp) == Receiver(g)(xpp) && uniqueActiveSender(g, x)
          case _ => true
        })
      } catch {
        case e: Receiver.UndefinedReceiverException => {
          System.err.println(e.toString)
          false
        }
      }
    }

    private def uniqueActiveSender(g: GlobalProtocol, x: String): Boolean = {
      try {
        ActiveSender(g, x)
        true
      } catch {
        case e: ActiveSender.NonChoiceException => {
          System.err.println(e.toString)
          false
        }
        case e: ActiveSender.NoActiveSenders => {
          System.err.println(e.toString)
          false
        }
      }
    }
  }

  object ActiveSender {

    class NonChoiceException(x: String) extends Exception {
      override def toString: String =
        "NonChoiceException: Active Sender is not defined for " + x
    }
    class NoActiveSenders(x: String) extends Exception {
      override def toString: String =
        "NoActiveSenders: No active senders found for " + x
    }

    def apply(g: GlobalProtocol, x: String) = {
      val (leftHash, rightHash) = g.getHashes
      leftHash(x) match {
        case Choice(x1, x2, x3) =>
          /**
           * At this point we don't know which is the correct singleton active
           * sender so we try them all.
           */
          implicit val justConmuted = false
          val activeSenders = g.getParticipants map
            (p => (p, reduce(g, x1, Set(), Set(p), x2, Set(), Set(p), 0))) filter (e => e._2)
          if (activeSenders.size != 1) {
            throw new NoActiveSenders(x)
          } else {
            activeSenders.last._1
          }
        case _ => throw new NonChoiceException(x)
      }

    }
    def reduce(g: GlobalProtocol, actL: String, setL: Set[String],
      parL: Set[String], actR: String, setR: Set[String],
      parR: Set[String], counter: Int): Boolean = {
      val (leftHash, rightHash) = g.getHashes
      leftHash(actL) match {
        /* Base cases */
        case End(x) if actL == actR => true
        case End(x) if setR.contains(actR) => true
        case _ if (setL.contains(actL) && setR.contains(actR)) => true
        case ChoiceJoin(x1, x2, x) if (x1 == actL && x2 == actR) || (x2 == actL && x1 == actR) => true
        case _ if (counter == 5) => reduce(g, actR, setR, parR, actL, setL, parL, 0)
        /* Recursive cases */
        case ParallelJoin(x1, x2, xp) if reduce(g, xp, setL, parL, actR, setR, parR, counter + 1) => true
        case ChoiceJoin(x1, x2, xp) if reduce(g, xp, setL, parL, actR, setR, parR, counter + 1) => true
        case Parallel(x1, x, xp) if reduce(g, x, setL + x1, parL, actR, setR, parR, counter + 1)
          && reduce(g, xp, setL + x1, parL, actR, setR, parR, counter + 1) => true
        case Choice(x1, x, xp) if reduce(g, x, setL + x1, parL, actR, setR, parR, counter + 1)
          && reduce(g, xp, setL + x1, parL, actR, setR, parR, counter + 1) => true
        case Message(x1, p, pp, _, _, x1p) if parL.contains(p)
          && reduce(g, x1p, setL + x1, parL + pp, actR, setR, parR, counter + 1) => true
        case _ => {
          if (counter == 0) false
          else {
            reduce(g, actR, setR, parR, actL, setL, parL, 0)
          }
        }
      }
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
}