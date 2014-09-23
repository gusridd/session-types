package lang

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import lang.aspect.GlobalAdvice
import lang.aspect.GlobalAspect

object ActiveSender {

  class NonChoiceException(x: String) extends Exception {
    override def toString: String =
      "NonChoiceException: Active Sender is not defined for " + x
  }
  class NoActiveSenders(x: String) extends Exception {
    override def toString: String =
      "NoActiveSenders: No active senders found for " + x
  }

  def apply(g: GlobalProtocol, x: String) = guess(g, x, reduce)
  def apply(a: GlobalAspect, x: String) = guess(a.adv, x, relaxedReduce)

  def guess(g: Global, x: String, recF: (Global, String, Set[String], Set[String], String, Set[String], Set[String], Int) => Boolean) = {
    val (leftHash, rightHash) = g.getHashes
    leftHash(x) match {
      case Choice(x, x1, x2) =>
        /**
         * At this point we don't know which is the correct singleton active
         * sender so we try them all. So we produce pairs of participants
         * and Boolean, if it reduces then (p,true) is given.
         */
        implicit val justConmuted = false
        //        println(g.getParticipants)
        //        println(g.getHashes()._1.foreach(x => println(x)))
        val activeSenders = g.getParticipants map
          (p => {
            //            println("ASend Participant: " + p)
            (p, recF(g, x1, Set(), Set(p), x2, Set(), Set(p), 0))
          }) filter (e => {
            if (e._2) {
              println("[INFO] ASend(" + x + "): " + e._1)
            }
            e._2
          })

        if (activeSenders.size != 1) {
          println("[FAIL] ASend(" + x + "): " + activeSenders)
          throw new NoActiveSenders(x)
        } else {
          activeSenders.last._1
        }
      case _ => throw new NonChoiceException(x)
    }
  }

  def reduce(g: Global, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], counter: Int): Boolean = {
    val (leftHash, rightHash) = g.getHashes

    //      println("***************")
    //      println("actL: " + actL)
    //      println("setL: " + setL)
    //      println("parL: " + parL)
    //      println("actR: " + actR)
    //      println("setR: " + setR)
    //      println("parR: " + parR)
    //      println("counter: " + counter)

    leftHash(actL) match {
      /* Base cases */
      case End(x) if actL == actR => true
      case End(x) if setR.contains(actR) => true
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
      case ChoiceJoin(x1, x2, x) if (x1 == actL && x2 == actR) || (x2 == actL && x1 == actR) => true
      /* Permutation rule heuristic */
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
      case Indirection(x1, x2) => reduce(g, x2, setL, parL, actR, setR, parR, counter)
      case _ => {
        if (counter == 0) false
        else {
          reduce(g, actR, setR, parR, actL, setL, parL, 0)
        }
      }
    }
  }

  /**
   * The Active Sender computation for an advice is relaxed as it has proceed
   * which do not carry information about who is sending or receiving a
   * particular message, but for projection tipically that information is not
   * needed, so a 'proceed'
   */
  def apply(a: GlobalAdvice, x: String) = {

  }

  def relaxedReduce(adv: Global, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], counter: Int): Boolean = {
    val (leftHash, rightHash) = adv.getHashes

    leftHash(actL) match {
      /* Base cases */
      case End(x) if actL == actR => true
      case End(x) if setR.contains(actR) => true
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
      case ChoiceJoin(x1, x2, x) if (x1 == actL && x2 == actR) || (x2 == actL && x1 == actR) => true
      /* Permutation rule heuristic */
      case _ if (counter == 5) => relaxedReduce(adv, actR, setR, parR, actL, setL, parL, 0)
      /* Recursive cases */
      case ParallelJoin(x1, x2, xp) if relaxedReduce(adv, xp, setL, parL, actR, setR, parR, counter + 1) => true
      case ChoiceJoin(x1, x2, xp) if relaxedReduce(adv, xp, setL, parL, actR, setR, parR, counter + 1) => true
      case Parallel(x1, x, xp) if relaxedReduce(adv, x, setL + x1, parL, actR, setR, parR, counter + 1)
        && relaxedReduce(adv, xp, setL + x1, parL, actR, setR, parR, counter + 1) => true
      case Choice(x1, x, xp) if relaxedReduce(adv, x, setL + x1, parL, actR, setR, parR, counter + 1)
        && relaxedReduce(adv, xp, setL + x1, parL, actR, setR, parR, counter + 1) => true
      /**
       * This is the only rule different from the normal reducing process, as
       * we only want to know if the decision is local to a participant and we
       * dont't care to check if the participants of both branches are informed
       * and can make decision on their own.
       */
      case Message(x1, p, pp, _, _, x1p) if parL.contains(p) => true
      case Indirection(x1, x2) => relaxedReduce(adv, x2, setL, parL, actR, setR, parR, counter)
      case _ => {
        if (counter == 0) false
        else {
          relaxedReduce(adv, actR, setR, parR, actL, setL, parL, 0)
        }
      }
    }
  }
}