package lang.aspect

import scala.annotation.tailrec
import lang.Choice
import lang.ChoiceJoin
import lang.End
import lang.GlobalProtocol
import lang.Indirection
import lang.LocalProtocol
import lang.LocalProtocol.Receive
import lang.LocalProtocol.Send
import lang.LocalProtocol.localExpr
import lang.Message
import lang.aspect.uniqueMsg.SimpleMessage
import lang.expr
import lang.Congruence
import lang.Parallel
import lang.GlobalProtocol
import lang.LocalProtocol.NullAction
import lang.Indirection
import lang.LocalProtocol.Fork
import lang.LocalProtocol.Merge

object Weaver {

  val numRegEx = """[0-9]+$""".r
  val regEx = """^x_[0-9]+$""".r

  @tailrec
  def naiveGlobalWeaving(aspects: List[GlobalAspect], g: GlobalProtocol): GlobalProtocol = {
    val exprs = g.exprs
    aspects match {
      case aspect :: aRest => {
        val (matches, rest) = exprs partition { e => pointcutMatchGlobal(aspect.pc, e) }
        naiveGlobalWeaving(aRest,
          new GlobalProtocol((matches flatMap ({
            case m @ Message(x, s, r, l, u, xp) => {
              /**
               *  The localize function is applied and the 'proceed' keyword
               *  is replaced by the actual message and
               *  end is replaced by xp, which is the ending
               *  variable from the message
               */

              (localize(aspect.adv, x).exprs map ({
                case AdviceTransition(x1, x2) => Message(x1, s, r, l, u, x2)
                case End(xe) => Indirection(xe, xp)
                case e => e
              })) :+ Indirection(x, format(x, aspect.xa))
            }
            case _ => throw new Exception("Weaving only matches messages")
          })) ++ rest, g.x_0))
      }
      case Nil => g
    }
  }

  def GlobalWeaving(aspects: List[GlobalAspect], g: GlobalProtocol): GlobalProtocol = {
    val exprs = g.exprs
    val r = aspects match {
      case aspect :: aRest => {
        val (matches, rest) = exprs partition { e => pointcutMatchGlobal(aspect.pc, e) }

        @tailrec
        def replaceMatch(ms: List[expr], all: Set[expr]): Set[expr] = {
          ms match {
            case m :: restm => m match {
              case m @ Message(x, s, r, l, u, xp) => {
                val Gap = tag(localize(aspect.adv, x), m, all.to)
                val upper = findUpper(((Gap.exprs ++ all)) flatMap (_.getVariables))

                val fx1 = "x_" + (upper + 1)
                val fx2 = "x_" + (upper + 2)
                val fx3 = "x_" + (upper + 3)

                val lp = freshLabel(aspect.adv.exprs ++ all, l)
                val newMessage = Message(fx1, s, r, lp, u, fx2)
                val newChoice = Choice(x, fx1, Gap.xa)
                val newMerge = ChoiceJoin(fx2, fx3, xp)

                val res = ((Gap.exprs map ({
                  case AdviceTransition(x1, x2) => Message(x1, s, r, l, u, x2)
                  case End(xe) => Indirection(xe, fx3)
                  case e => e
                })) ++ List(newMessage, newChoice, newMerge))

                val set: Set[expr] = (res ++ all).to

                replaceMatch(restm, set.to)
              }
              case _ => throw new Exception("Weaving should only match messages")
            }
            case Nil => all
          }
        }

        (replaceMatch(matches, exprs.to) -- matches).to
      }
      case Nil => exprs
    }
    new GlobalProtocol(r.to, g.x_0)
  }

  def localWeaving(aspects: List[LocalAspect], lp: LocalProtocol): LocalProtocol = {
    aspects match {
      case aspect :: aRest => {
        val pc = Congruence(aspect.pc)

        // T-Daemon
        if (pc == NullPC()) {
          val upper = findUpper((aspect.adv.exprs ++ lp.exprs) flatMap (_.getVariables))
          val fx0 = "x_" + (upper + 1)
          val fx1 = "x_" + (upper + 2)
          val fxe = "x_" + (upper + 3)

          val nParallel = Fork(fx0, lp.x_0, fx1)
          val nChoiceJoin = Merge(fx1, fxe, aspect.xa)
          val replacedTa: List[localExpr] = aspect.adv.exprs map {
            case LocalProtocol.AdviceTransition(x1, x2) => LocalProtocol.NullAction(x1, x2)
            case LocalProtocol.End(x) => LocalProtocol.Indirection(x, fxe)
            case e => e
          }
          return new LocalProtocol(replacedTa ++ List(nParallel, nChoiceJoin), lp.p, fx0)
        }

        // T-NoDaemon
        val (matches, rest) = lp.exprs partition { e => pointcutMatchLocal(pc, e) }

        localWeaving(aRest,
          new LocalProtocol((matches flatMap ({
            case Send(x, p, l, u, xp) => {
              /**
               *  The localize function is applied and the 'proceed' keyword
               *  is replaced by the actual message and
               *  end is replaced by xp, which is the ending
               *  variable from the message
               */
              (localize(aspect.adv, x).exprs map ({
                case LocalProtocol.AdviceTransition(x1, x2) => Send(x1, p, l, u, x2)
                case LocalProtocol.End(xe) => LocalProtocol.Indirection(xe, xp)
                case e => e
              })) :+ LocalProtocol.Indirection(x, format(x, aspect.xa))
            }
            case Receive(x, p, l, u, xp) => {
              /**
               * The same as above
               */
              (localize(aspect.adv, x).exprs map ({
                case LocalProtocol.AdviceTransition(x1, x2) => Send(x1, p, l, u, x2)
                case LocalProtocol.End(xe) => LocalProtocol.Indirection(xe, xp)
                case e => e
              })) :+ LocalProtocol.Indirection(x, format(x, aspect.xa))
            }
            case _ => throw new Exception("Weaving only matches messages")
          })) ++ rest, lp.p, lp.x_0))

        def replaceMatch(ms: List[localExpr], all: Set[localExpr]): Set[localExpr] = {
          ms match {
            case m :: restm => m match {
              //              case s @ Send(x, p, l, u, xp) => {
              //
              //              }
              //              case r @ Receive(x, p, l, u, xp) => {
              //
              //              }
              case _ => all
              case _ => throw new Exception("Weaving should only match messages")
            }
            case Nil => all
          }
        }
        new LocalProtocol(((replaceMatch(matches, lp.exprs.to) -- matches)).to, lp.p, lp.x_0)
      }
      case Nil => lp
    }
  }

  /**
   * Pointcut Matching
   */
  def pointcutMatchGlobal(pc: GlobalPointcut, e: expr) = pc.doesMatch(e)

  def pointcutMatchLocal(pc: LocalPointcut, e: localExpr): Boolean = pc.doesMatch(e)

  /**
   * Function responsible for maintaining the uniqueness of states
   * when the advice is inserted several times within the same
   * global session.
   */
  private[this] def localize[A <: Advice[A]](adv: A, x: String): A = {
    val xs: Set[String] = (adv.exprs flatMap (_.getVariables)).to

    @tailrec def loc(adv: A, replacements: List[String]): A =
      replacements match {
        case xp :: xps => {
          val er = adv.substitute(xp, format(x, xp))
          loc(adv.substitute(xp, format(x, xp)), xps)
        }
        case Nil => adv
      }

    loc(adv, xs.to)
  }

  private[this] def tag(adv: GlobalAdvice, m: SimpleMessage, exprs: List[expr]) = {
    val aExprs: List[expr] = adv.exprs
    val F = labels(aExprs) diff labels(exprs)
    new GlobalAdvice(aExprs map {
      case Message(x, s, r, l, u, xp) if (F.contains(l)) =>
        Message(x, s, r, l + "^[" + m.canonical() + "]", u, xp)
      case e => e
    }, adv.xa)
  }

  private[this] def labels(exprs: List[expr]): Set[String] = {
    (exprs flatMap {
      case Message(x, s, r, l, t, xp) => Some(l)
      case _ => None
    }).to
  }

  private[this] def format(x: String, xp: String): String = xp + "^[" + x + "]"

  /**
   * This function replaces all variable names generated by the weaving process
   * like x_3^[x_2] by normal variables such as x_4.
   */
  def normalize(adv: GlobalAdvice, exprs: List[expr]) = {
    val variableNames = (exprs flatMap (_.getVariables))

    val (regular, woven) = variableNames partition (regEx.findFirstIn(_) match {
      case Some(_) => true
      case None => false
    })

    val upper = findUpper(variableNames)

    val newVars = (List.range(upper, upper + woven.size)) map ("x_" + _)

    val replacements = woven.zip(newVars)

    val newXa: String = replacements find ({ case (l, r) => l == adv.xa }) match {
      case Some((l, r)) => r
      case None => ""
    }

    new GlobalAdvice(replacements.foldRight(exprs)((repl: (String, String), es: List[expr]) => {
      repl match {
        case (s1, s2) => es map { e: expr => e.substitute(s1, s2) }
      }
    }), newXa)
  }

  private[this] def findUpper(strs: List[String]): Int = {
    (strs map (x => {
      /*println(x);*/ numRegEx.findFirstIn(x) match {
        case Some(nS) => nS.toInt
        case None => -1
      }
    })).max + 1
  }

  @tailrec
  private[this] def freshLabel(exprs: List[expr], l: String): String = {
    val labels = ((exprs flatMap {
      case m @ Message(_, _, _, _, _, _) => Some(m)
      case _ => None
    }) map ({
      case Message(_, _, _, l, _, _) => l
    })).toSet
    if (labels.contains(l)) {
      freshLabel(exprs, l + "'")
    } else {
      l
    }
  }

}