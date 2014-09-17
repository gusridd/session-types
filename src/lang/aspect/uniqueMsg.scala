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

object uniqueMsg {
  /**
   * Main method that tries to check
   */
    def apply(g: GlobalProtocol, a: GlobalAspect): Boolean = {
  
      val leftHash = a.adv.getHashes._1
      def Msg(x: String, xb: Set[String], M: Set[SimpleMessage]): Boolean = {
        if(x == "x_0" && !leftHash.isDefinedAt(x)){
          a.adv.exprs foreach {e => println(e.canonical)}
          println("FAILFAILFAILFAIL")
          println(leftHash)
        }
        leftHash(x) match {
          case m @ Message(x, _, _, _, _, xp) => {
            val Mp = M - m
            if (M.contains(m)) {
              // The disjoint sum of sets considers the empty set
              val r = Msg(xp, xb, Mp) // || Msg(xp, xb, M)
              if (r) {
                println("[Msg-Message]:")
                println("\t" + m.canonical)
                println("\tMsg(" + xp + ", " + xb + ", " + Mp + ")")
              }
              r
            } else {
              false
            }
          }
          case AdviceTransition(x1, x2) => {
            val pairs = disjointPartition(M)
  
            // This represent all the disjoint sums in whose all the elements
            // of the first set are matched by some pointcut.
            val properties = pairs filter ({
              case (sM, sMP) => sM forall (m => Weaver.pointcutMatchGlobal(a.pc, m))
            })
            properties exists ({
              case (m, mp) => {
                val r = Msg(x2, xb, mp)
                if (r) {
                  println("[Msg-Proceed]:")
                  println("\t" + leftHash(x2).canonical)
                  println("\tMsg(" + x2 + ", " + xb + ", " + mp + ")")
                }
                r
              }
            })
          }
          case c @ Choice(x, x1, x2) if (!xb.contains(x)) => {
            sumPartition(M) exists ({
              case (m1, m2) => {
                val r = Msg(x1, xb + x, m1) && Msg(x2, xb + x, m2)
                if (r) {
                  println("[Msg-Choice]:")
                  println("\t" + c.canonical)
                  println("\tMsg(" + x1 + ", " + (xb + x) + ", " + m1 + ")")
                  println("\tMsg(" + x2 + ", " + (xb + x) + ", " + m2 + ")")
                }
                r
              }
            })
          }
          case p @ Parallel(x, x1, x2) if (!xb.contains(x)) => {
            disjointPartition(M) exists ({
              case (m1, m2) => {
                val r = Msg(x1, xb + x, m1) && Msg(x2, xb + x, m2)
                if (r) {
                  println("[Msg-Fork]:")
                  println("\t" + p.canonical)
                  println("\tMsg(" + x1 + ", " + (xb + x) + ", " + m1 + ")")
                  println("\tMsg(" + x2 + ", " + (xb + x) + ", " + m2 + ")")
                }
                r
              }
            })
          }
          // As in the ChoiceJoin and ParallelJoin x1 and x2 appear both at the 
          // left side of the equation, no other case should be checked.
          case c @ ChoiceJoin(x1, x2, x) => {
            val r = Msg(x, xb, M)
            if (r) {
              println("[Msg-Merge/Join]:")
              println("\t" + c.canonical)
              println("\tMsg(" + x + ", " + xb + ", " + M + ")")
            }
            r
          }
          case p @ ParallelJoin(x1, x2, x) => {
            val r = Msg(x, xb, M)
            if (r) {
              println("[Msg-Merge/Join]:")
              println("\t" + p.canonical)
              println("\tMsg(" + x + ", " + xb + ", " + M + ")")
            }
            r
          }
          // Base cases
          case c @ Choice(x, x1, x2) if (M.isEmpty && xb.contains(x)) => {
            println("[Msg-Choice/Fork-Stop]:")
            println("\t" + c.canonical)
            println("\tMsg(" + x + ", " + xb + ", " + M + ")")
            true
          }
          case p @ Parallel(x, x1, x2) if (M.isEmpty && xb.contains(x)) => {
            println("[Msg-Choice/Fork-Stop]:")
            println("\t" + p.canonical)
            println("\tMsg(" + x + ", " + xb + ", " + M + ")")
            true
          }
          case e @ End(x) if (M.isEmpty) => {
            println("[Msg-End]:")
            println("\t" + e.canonical)
            println("\tMsg(" + x + ", " + xb + ", " + M + ")")
            true
          }
          case End(x) => false
          case Choice(x, x1, x2) => false
          case Parallel(x, x1, x2) => false
        }
      }
  
      /**
       * If we try to compute the Msg function starting with every message
       * it should give true in every case.
       */
      val messages: Set[SimpleMessage] = ((a.adv.exprs ++ g.exprs) flatMap {
        case m @ Message(_, s, r, l, u, _) => Some(SimpleMessage(s, r, l, u))
        case _ => None
      }).to
      //    (a.adv.exprs ++ g.exprs) flatMap {
      //      case m @ Message(_, _, _, _, _, _) => Some(m)
      //      case _ => None
      //    } forall (M => Msg("x_0", Set(), messages))
      messages.subsets exists (M => {
        val r = Msg("x_0", Set(), M)
        if (r) println("The correct set M for UniqueMsg was: " + M)
        r
      })
      //    Msg("x_0", Set(), messages)
    }

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

//  def apply(g: GlobalProtocol, a: Aspect): Boolean = {
//    val leftHash = a.adv.getHashes._1
//    def Msg(x: String, xb: Set[String], M: Set[SimpleMessage], lastM: Set[SimpleMessage]): Boolean = {
//      leftHash(x) match {
//        case m @ Message(x, _, _, _, _, xp) => {
//          val Mp = M - m
//          if (M.contains(m)) {
//            // The disjoint sum of sets considers the empty set
//            val r = Msg(xp, xb, Mp, lastM) // || Msg(xp, xb, M)
//            if (r) {
//              println("[Msg-Message]:")
//              println("\t" + m.canonical)
//              println("\tMsg(" + xp + ", " + xb + ", " + Mp + ", " + lastM + ")")
//            }
//            r
//          } else {
//            false
//          }
//        }
//        case AdviceTransition(x1, x2) => {
//          val pairs = disjointPartition(M)
//
//          // This represent all the disjoint sums in whose all the elements
//          // of the first set are matched by some pointcut.
//          val properties = pairs filter ({
//            case (sM, sMP) => sM forall (m => Weaver.pointcutMatchGlobal(a.pc, m))
//          })
//          properties exists ({
//            case (m, mp) => {
//              val r = Msg(x2, xb, mp, lastM)
//              if (r) {
//                println("[Msg-Proceed]:")
//                println("\t" + leftHash(x2).canonical)
//                println("\tMsg(" + x2 + ", " + xb + ", " + mp + ", " + lastM + ")")
//              }
//              r
//            }
//          })
//        }
//        case c @ Choice(x, x1, x2) if (!xb.contains(x)) => {
//          sumPartition(M) exists ({
//            case (m1, m2) => {
//              val r = Msg(x1, xb + x, m1, lastM) && Msg(x2, xb + x, m2, lastM)
//              if (r) {
//                println("[Msg-Choice]:")
//                println("\t" + c.canonical)
//                println("\tMsg(" + x1 + ", " + (xb + x) + ", " + m1 + ", " + lastM + ")")
//                println("\tMsg(" + x2 + ", " + (xb + x) + ", " + m2 + ", " + lastM + ")")
//              }
//              r
//            }
//          })
//        }
//        case p @ Parallel(x, x1, x2) if (!xb.contains(x)) => {
//          disjointPartition(M) exists ({
//            case (m1, m2) => {
//              val r = Msg(x1, xb + x, m1, lastM) && Msg(x2, xb + x, m2, lastM)
//              if (r) {
//                println("[Msg-Fork]:")
//                println("\t" + p.canonical)
//                println("\tMsg(" + x1 + ", " + (xb + x) + ", " + m1 + ", " + lastM + ")")
//                println("\tMsg(" + x2 + ", " + (xb + x) + ", " + m2 + ", " + lastM + ")")
//              }
//              r
//            }
//          })
//        }
//        // As in the ChoiceJoin and ParallelJoin x1 and x2 appear both at the 
//        // left side of the equation, no other case should be checked.
//        case c @ ChoiceJoin(x1, x2, x) => {
//          val r = Msg(x, xb, M, lastM)
//          if (r) {
//            println("[Msg-Merge/Join]:")
//            println("\t" + c.canonical)
//            println("\tMsg(" + x + ", " + xb + ", " + M + ", " + lastM + ")")
//          }
//          r
//        }
//        case p @ ParallelJoin(x1, x2, x) => {
//          val r = Msg(x, xb, M, lastM)
//          if (r) {
//            println("[Msg-Merge/Join]:")
//            println("\t" + p.canonical)
//            println("\tMsg(" + x + ", " + xb + ", " + M + ", " + lastM + ")")
//          }
//          r
//        }
//        // Base cases
//        case c @ Choice(x, x1, x2) if (M.isEmpty && xb.contains(x)) => {
//          println("[Msg-Choice/Fork-Stop]:")
//          println("\t" + c.canonical)
//          println("\tMsg(" + x + ", " + xb + ", " + M + ", " + lastM + ")")
//          true
//        }
//        case p @ Parallel(x, x1, x2) if (M.isEmpty && xb.contains(x)) => {
//          println("[Msg-Choice/Fork-Stop]:")
//          println("\t" + p.canonical)
//          println("\tMsg(" + x + ", " + xb + ", " + M + ", " + lastM + ")")
//          true
//        }
//        case e @ End(x) if (M.isEmpty) => {
//          println("[Msg-End]:")
//          println("\t" + e.canonical)
//          println("\tMsg(" + x + ", " + xb + ", " + M + ", " + lastM + ")")
//          true
//        }
//        case End(x) => false
//        case Choice(x, x1, x2) => false
//        case Parallel(x, x1, x2) => false
//      }
//    }
//
//    /**
//     * If we try to compute the Msg function starting with every message
//     * it should give true in every case.
//     */
//    val messages: Set[SimpleMessage] = ((a.adv.exprs ++ g.exprs) flatMap {
//      case m @ Message(_, s, r, l, u, _) => Some(SimpleMessage(s, r, l, u))
//      case _ => None
//    }).to
//    //    (a.adv.exprs ++ g.exprs) flatMap {
//    //      case m @ Message(_, _, _, _, _, _) => Some(m)
//    //      case _ => None
//    //    } forall (M => Msg("x_0", Set(), messages))
//    messages.subsets exists (auxM => {
//      disjointPartition(auxM) exists ({
//        case (ms, lastM) => {
//          val r = Msg("x_0", Set(), ms, lastM)
//          if (r) {
//            println("The correct sets for UniqueMsg were")
//            println("M: " + ms)
//            println("lastM " + lastM)
//          }
//          r
//        }
//      })
//    })
//    //    Msg("x_0", Set(), messages)
//  }

}