package lang.aspect

import lang.GlobalProtocol
import lang.Message
import lang.expr
import lang.End
import lang.LocalProtocol.Merge
import lang.LocalProtocol.Join
import lang.LocalProtocol.Fork
import lang.LocalProtocol.InternalChoice
import lang.LocalProtocol.ExternalChoice
import lang.LocalProtocol.Receive
import lang.LocalProtocol.Send
import lang.aspect.uniqueMsg.SimpleMessage

/**
 * This condition is true, if for all M such that match(pc,M), adv is
 * single-threaded wrt M
 */
object SgTh {

  val bs = Set(true, false)

  def apply(g: GlobalProtocol, a: Aspect): Boolean = {
    val matches = g.exprs filter { e => Weaver.pointcutMatchGlobal(a.pc, e) }

    matches forall {
      case m @ Message(x1, p, pp, l, u, x2) => {
    	  
        // adv[proceed -> M]
        val replacedAspect = Aspect(a.name, a.pc, Advice(a.adv.exprs map {
          case AdviceTransition(x1, x2) => Message(x1, p, pp, l, u, x2)
          case e => e
        }, a.adv.xa))

        // Projections
        val Ta_p = LocalProjection(replacedAspect, p)
        val Ta_pp = LocalProjection(replacedAspect, pp)

        val end_p = Ta_p.adv.exprs.flatMap({
          case e @ End(_) => List(e)
          case _ => List()
        })
        val end_pp = Ta_pp.adv.exprs.flatMap({
          case e @ End(_) => List(e)
          case _ => List()
        })
        if (end_p.size != 1) {
          println("[EXCEPTION] The aspect have an end amount != 1")
          throw new Exception("The aspect have an end amount != 1")
        }
        if (end_pp.size != 1) {
          println("[EXCEPTION] The aspect have an end amount != 1")
          throw new Exception("The aspect have an end amount != 1")
        }
        val STh_p = bs exists ({ b => STh(Ta_p, m, "end", List(), b) })
        val STh_pp = bs exists ({ b => STh(Ta_pp, m, "end", List(), b) })

        false
      }
    }
  }

  private[this] def STh(la: LocalAspect, m: SimpleMessage, x: String, xb: List[String], b: Boolean): Boolean = {
    val (lHash, rHash) = la.adv.getHashes

    // STh-Init
    if (x == la.xa) return true

    rHash(x) match {
      // STh-End
      case End(x) => STh(la, m, x, xb, b)
      // STh-Merge/Join-Stop
      case Merge(x1, x2, x3) if (b == false && xb.contains(x)) => true
      case Join(x1, x2, x3) if (b == false && xb.contains(x)) => true
      // STh-Fork/Choice
      case InternalChoice(x1, x2, x3) => STh(la, m, x1, xb, b)
      case ExternalChoice(x1, x2, x3) => STh(la, m, x1, xb, b)
      case Fork(x1, x2, x3) => STh(la, m, x1, xb, b)
      // STh-Join
      case Join(x1, x2, x3) => disjunction exists {
        case (b1, b2) => STh(la, m, x1, xb :+ x, b1) && STh(la, m, x2, xb :+ x, b2)
      }
      // STh-Merge
      case Merge(x1, x2, x3) if (!xb.contains(x)) => or(b) exists {
        case (b1, b2) => STh(la, m, x1, xb :+ x, b1) && STh(la, m, x2, xb :+ x, b2)
      }
      case Receive(x1, q, l, u, x2) => m match {
        // STh-Message2
        case SimpleMessage(p, l, u, pp) if (q == p || q == pp) => STh(la, m, x1, xb, b)
        // STh-Message
        case SimpleMessage(p, l, u, pp) if (q != p && q != pp) => true
      }
      case Send(x1, q, l, u, x2) => m match {
        // STh-Message2
        case SimpleMessage(p, l, u, pp) if (q == p || q == pp) => STh(la, m, x1, xb, b)
        // STh-Message
        case SimpleMessage(p, l, u, pp) if (q != p && q != pp) => true
      }
    }

    false
  }
  /**
   * THe disjunction operator # is undefined when the two arguments are both
   * true. In this case is represented at all combinations of a pair of booleans
   * which comply with that.
   */
  private[this] def disjunction = for {
    b1 <- bs
    b2 <- bs
    if (!b1 || !b2)
  } yield (b1, b2)

  /**
   * Computes the pair of booleans that their or gives the parameter b
   */
  private[this] def or(b: Boolean) = for {
    b1 <- bs
    b2 <- bs
    if (b1 || b2)
  } yield (b1, b2)
}