package lang.aspect

import scala.Option.option2Iterable
import lang.Message
import lang.Choice
import lang.Parallel
import lang.End
import lang.ChoiceJoin
import lang.ParallelJoin
import lang.GlobalProtocol
import scala.collection.mutable.HashMap
import lang.expr

object uniqueMsg {
  /**
   * Main method that tries to check
   */

  type iSet[T] = scala.collection.immutable.Set[T]

  def disjointPartition[M](s: Set[M]): Iterator[(iSet[M], iSet[M])] =
    for {
      s1 <- s.subsets
    } yield (s1, s -- s1)

  def sumPartition[M](s: Set[M]): Iterator[(iSet[M], iSet[M])] = {
    for {
      s1 <- s.subsets
      s2 <- s.subsets
      if (s1 ++ s2 == s)
    } yield (s1, s2)
  }

  case class SimpleMessage(s: String, r: String, l: String, u: String) {
    def canonical() = u match {
      case "" => s + " -> " + r + ": " + l
      case _ => s + " -> " + r + ": " + l + "(" + u + ")"
    }
  }

  implicit def messageToSimpleMessage(m: Message): SimpleMessage = m match {
    case Message(_, s, r, l, u, _) => SimpleMessage(s, r, l, u)
  }

  implicit def simpleMessageToMessage(sm: SimpleMessage): Message = sm match {
    case SimpleMessage(s, r, l, u) => Message("", s, r, l, u, "")
  }

  def apply(g: GlobalProtocol, a: GlobalAspect): Boolean = {

    val (lHash, rHash) = a.adv.getHashes

    val advParticipants = a.adv.getParticipants
    val pcParticipants = a.pc.getParticipants
    /**
     * There can exist more than one daemon participant
     */
    val dParticipants = advParticipants -- pcParticipants
    println("[INFO] Daemon participants " + dParticipants)

    def collectFirstMessagesToDaemon(x: String, cs: Set[SimpleMessage], mp: Set[String], ds: Set[String]): Set[SimpleMessage] = {
      lHash(x) match {
        case m @ Message(_, s, r, _, _, xp) if (dParticipants.contains(r) && !ds.contains(r)) =>
          collectFirstMessagesToDaemon(xp, cs + m, mp, ds + r)
        case m @ Message(_, s, r, _, _, xp) => collectFirstMessagesToDaemon(xp, cs, mp, ds)
        case Choice(_, x1, x2) =>
          collectFirstMessagesToDaemon(x1, cs, mp, ds) ++ collectFirstMessagesToDaemon(x2, cs, mp, ds)
        case ChoiceJoin(_, _, xm) if (mp.contains(xm)) => cs
        case ChoiceJoin(_, _, xm) =>
          collectFirstMessagesToDaemon(xm, cs, mp + xm, ds)
        case Parallel(_, x1, x2) =>
          collectFirstMessagesToDaemon(x1, cs, mp, ds) ++ collectFirstMessagesToDaemon(x2, cs, mp, ds)
        case ParallelJoin(_, _, xp) => collectFirstMessagesToDaemon(xp, cs, mp, ds)
        /**
         * We know that if a participant is considered as a daemon, then it does
         * not appear in the pointcut, therefore the 'proceed' can't be a
         * message to a daemon.
         */
        case AdviceTransition(_, xp) => collectFirstMessagesToDaemon(xp, cs, mp, ds)
        case End(x) => cs
      }
    }
    val firstToDaemon = collectFirstMessagesToDaemon(a.xa, Set(), Set(), Set())

    println("[INFO] firstToDaemon " + firstToDaemon)

    /**
     * This function checks if for each branch in the interaction, each daemon
     * is notified of the start of the interaction by messages on the set
     * computed with 'collectFirstMessagesToDaemon', and only one can occur on
     * each branch, if now, they can be confused with parallel interactions from
     * the original session. It collects the choice points because the interaction
     * can start with a merge, which can repeat initial messages, so choices are
     * used to avoid infinite recursion. This is also possible because an aspect
     * should have an end, so if an initial merge is found, at least one choice
     * exist that will avoid the problem.
     */
    def justOccurOnce(x: String, someSeen: Boolean, ftd: Set[SimpleMessage], chp: Set[String]): Boolean = {
      println("[INFO] seen: " + someSeen + " m: " + lHash(x).canonical)
      lHash(x) match {
        case m @ Message(_, s, r, _, _, xp) if (someSeen && ftd.contains(m)) => false
        case m @ Message(_, s, r, _, _, xp) if (!someSeen && ftd.contains(m)) => justOccurOnce(xp, true, ftd, chp)
        case m @ Message(_, s, r, _, _, xp) => justOccurOnce(xp, someSeen, ftd, chp)
        case Choice(_, x1, x2) if (chp.contains(x)) => true
        case Choice(_, x1, x2) if (!chp.contains(x)) => justOccurOnce(x1, someSeen, ftd, chp + x) && justOccurOnce(x2, someSeen, ftd, chp + x)
        case ChoiceJoin(_, _, xp) => justOccurOnce(xp, someSeen, ftd, chp + xp)
        case Parallel(_, x1, x2) => justOccurOnce(x1, someSeen, ftd, chp) && justOccurOnce(x2, someSeen, ftd, chp)
        case ParallelJoin(_, _, xp) => justOccurOnce(xp, someSeen, ftd, chp)
        case AdviceTransition(_, xp) => justOccurOnce(xp, someSeen, ftd, chp)
        case End(_) => true
      }
    }

    justOccurOnce(a.xa, false, firstToDaemon, Set())
  }

}