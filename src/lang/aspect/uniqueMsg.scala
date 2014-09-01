package lang.aspect

import scala.Option.option2Iterable
import lang.Message
import lang.Choice
import lang.Parallel
import lang.End
import lang.ChoiceJoin
import lang.ParallelJoin

object uniqueMsg {
  /**
   * Main method that tries to check
   */
  def apply(a: Aspect): Boolean = {

    val (leftHash, rightHash) = a.adv.getHashes
      def Msg(x: String, xb: Set[String], M: Set[Message]): Boolean = {
        leftHash(x) match {
          case m @ Message(x, _, _, _, _, xp) => {
            val Mp = M - m
            // The disjoint sum of sets considers the empty set
            Msg(xp, xb, Mp) || Msg(xp, xb, M)
          }
          case AdviceTransition(x1, x2) => {
            val pairs = (M.subsets map (s => (s, M -- s)))

            // This represent all the disjoint sums in whose all the elements
            // of the first set are matched by some pointcut.
            val properties = pairs filter ({
              case (sM, sMP) => sM forall (m => Weaver.pointcutMatchGlobal(a.pc, m))
            })
            properties exists ({
              case (m, mp) => Msg(x2, xb, mp)
            })
          }
          case Choice(x, x1, x2) if (!xb.contains(x)) => {
            sumPartition(M) exists ({
              case (m1, m2) => Msg(x1, xb + x, m1) && Msg(x2, xb + x, m2)
            })
          }
          case Parallel(x, x1, x2) if (!xb.contains(x)) => {
            disjointPartition(M) exists ({
              case (m1, m2) => Msg(x1, xb + x, m1) && Msg(x2, xb + x, m2)
            })
          }
          // As in the ChoiceJoin and ParallelJoin x1 and x2 appear both at the 
          // left side of the equation, no other case should be checked.
          case ChoiceJoin(x1,x2,x) => Msg(x,xb,M)
          case ParallelJoin(x1,x2,x) => Msg(x,xb,M)
          
          // Base cases
          case Choice(x, x1, x2) if (xb.contains(x)) => true
          case Parallel(x, x1, x2) if (xb.contains(x)) => true
          case End(x) => true
        }
      }

    /**
     * We try to find to start the computation assuming that every message
     * could be unique as we don't know from the start which one could be.
     */
    val uniqueMessages = a.adv.exprs flatMap {
      case m @ Message(_, _, _, _, _, _) => Some(m)
      case _ => None
    } map (M => Msg("x_0", Set(), Set(M)))

    uniqueMessages.size == 1
  }

  type iSet[T] = scala.collection.immutable.Set[T]

  def disjointPartition(s: Set[Message]): Iterator[(iSet[Message], iSet[Message])] =
    for {
      s1 <- s.subsets
    } yield (s1, s -- s1)

  def sumPartition(s: Set[Message]): Iterator[(iSet[Message], iSet[Message])] = {
    for {
      s1 <- s.subsets
      s2 <- s.subsets
      if (s1 ++ s2 == s)
    } yield (s1, s2)
  }

}