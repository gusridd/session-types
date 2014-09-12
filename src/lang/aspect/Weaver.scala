package lang.aspect

import lang.GlobalProtocol
import lang.expr
import lang.Message
import lang.End
import lang.Indirection
import lang.LocalProtocol.Send
import scala.annotation.tailrec
import lang.aspect.uniqueMsg.SimpleMessage

object Weaver {

  @tailrec
  def naiveGlobalWeaving(aspects: List[Aspect], exprs: List[expr]): List[expr] = aspects match {
    case aspect :: aRest => {
      val (matches, rest) = exprs partition { e => pointcutMatchGlobal(aspect.pc, e) }
      naiveGlobalWeaving(aRest,
        (matches flatMap ({
          case m @ Message(x, s, r, l, u, xp) => {
            /**
             *  The localize function is applied and the 'proceed' keyword
             *  is replaced by the actual message and
             *  end is replaced by xp, which is the ending
             *  variable from the message
             */

            (localize(aspect.adv, x) map ({
              case AdviceTransition(x1, x2) => Message(x1, s, r, l, u, x2)
              case End(xe) => Indirection(xe, xp)
              case e => e
            })) :+ Indirection(x, format(x, "x_0"))
          }
          case _ => throw new Exception("Weaving only matches messages")
        })) ++ rest)
    }
    case Nil => exprs
  }

  def GlobalWeaving(aspects: List[Aspect], exprs: List[expr]): List[expr] = aspects match {
    case aspect :: aRest => {
      val (matches, rest) = exprs partition { e => pointcutMatchGlobal(aspect.pc, e) }
      GlobalWeaving(aRest,
        (matches flatMap ({
          case m @ Message(x, s, r, l, u, xp) => {
            /**
             *  Tagging is made and the 'proceed' keyword
             *  is replaced by the actual message and
             *  end is replaced by xp, which is the ending
             *  variable from the message
             */

            (tag(localize(aspect.adv, x), m, exprs) map ({
              case AdviceTransition(x1, x2) => Message(x1, s, r, l, u, x2)
              case End(xe) => Indirection(xe, xp)
              case e => e
            })) :+ Indirection(x, format(x, "x_0"))
          }
          case _ => throw new Exception("Weaving only matches messages")
        })) ++ rest)
    }
    case Nil => exprs
  }

  /**
   * Pointcut Matching
   */
  def pointcutMatchGlobal(pcs: List[Pointcut], e: expr) = e match {
    case Message(x1, s1, r1, l1, t1, x2) => pcs exists {
      case Pointcut(s2, r2, l2, t2) if (s1 == s2 && r1 == r2) =>
        l2 == "*" || (l2 == l1 && (t2 == "*" || t2 == t1))
      case _ => false
    }
    case _ => false
  }

  /**
   * Function responsible for maintaining the uniqueness of states
   * when the advice is inserted several times within the same
   * global session.
   */
  private[this] def localize(adv: Advice, x: String): List[expr] = {
    val xs: Set[String] = (adv.exprs flatMap (_.getVariables)).to

    @tailrec def loc(exprs: List[expr], replacements: List[String]): List[expr] =
      replacements match {
        case xp :: xps => loc(exprs map (_.substitute(xp, format(x, xp))), xps)
        case Nil => exprs
      }

    loc(adv.exprs, xs.to)
  }

  private[this] def tag(aExprs: List[expr], m: SimpleMessage, exprs: List[expr]) = {
    val F = labels(aExprs) diff labels(exprs)
    aExprs map {
      case Message(x, s, r, l, u, xp) if (F.contains(l)) =>
        Message(x, s, r, l + "^[" + m.canonical() + "]", u, xp)
      case e => e
    }
  }

  private[this] def labels(exprs: List[expr]): Set[String] = {
    (exprs flatMap {
      case Message(x, s, r, l, t, xp) => Some(l)
      case _ => None
    }).to
  }

  private[this] def format(x: String, xp: String): String = xp + "^[" + x + "]"

}