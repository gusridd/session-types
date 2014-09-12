package lang.aspect

import lang.GlobalProtocol
import lang.expr
import lang.Message
import lang.End
import lang.Indirection
import lang.LocalProtocol.Send
import scala.annotation.tailrec
import lang.aspect.uniqueMsg.SimpleMessage
import lang.Choice
import lang.ChoiceJoin

object Weaver {

  val numRegEx = """[0-9]+$""".r
  val regEx = """^x_[0-9]+$""".r

  @tailrec
  def naiveGlobalWeaving(aspects: List[Aspect], exprs: List[expr]): List[expr] =
    aspects match {
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

              (localize(aspect.adv, x).exprs map ({
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
      
      val r = ((matches flatMap ({
        case m @ Message(x, s, r, l, u, xp) => {
          /**
           *  Tagging is made, the 'proceed' keyword
           *  is replaced by the actual message and
           *  'end' is replaced by 'xp', which is the ending
           *  variable from the message
           */

          val Gap = tag(localize(aspect.adv, x), m, exprs)
          //            println(Gap)
          //            println(Gap.exprs ++ exprs)
          // val nGap = normalize(Gap)
          val upper = findUpper(((Gap.exprs ++ exprs)) flatMap (_.getVariables))
          println("upper " + upper)
          val fx1 = "x_" + (upper + 1)
          val fx2 = "x_" + (upper + 2)
          val fx3 = "x_" + (upper + 3)
          val lp = freshLabel(aspect.adv.exprs ++ exprs, l)
          println("fLabel: " + lp)

          val newMessage = Message(fx1, s, r, lp, u, fx2)
          val newChoice = Choice(x, fx1, Gap.xa)
          val newMerge = ChoiceJoin(fx2, fx3, xp)

          println("newMessage: " + newMessage)
          println("newChoice: " + newChoice)
          println("newMerge: " + newMerge)

          val res = ((Gap.exprs map ({
            case AdviceTransition(x1, x2) => Message(x1, s, r, l, u, x2)
            case End(xe) => Indirection(xe, fx3)
            case e => e
          })) ++ List(newMessage, newChoice, newMerge))
          println("--------------------------")
          res foreach (e => println(e.canonical))
          println("--------------------------")
          res
        }
        case _ =>
          throw new Exception("Weaving should only match messages")

      })) ++ rest)
      
      matches.foldRight(exprs)((e: expr, es: List[expr]) => e match {
        case m @ Message(x, s, r, l, u, xp) => es
        case _ => throw new Exception("Weaving should only match messages") 
      })
      
      GlobalWeaving(aRest, r)
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
  private[this] def localize(adv: Advice, x: String): Advice = {
    val xs: Set[String] = (adv.exprs flatMap (_.getVariables)).to

    @tailrec def loc(adv: Advice, replacements: List[String]): Advice =
      replacements match {
        case xp :: xps => loc(adv.substitute(xp, format(x, xp)), xps)
        case Nil => adv
      }

    loc(adv, xs.to)
  }

  private[this] def tag(adv: Advice, m: SimpleMessage, exprs: List[expr]) = {
    val aExprs: List[expr] = adv.exprs
    val F = labels(aExprs) diff labels(exprs)
    new Advice(aExprs map {
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
  def normalize(adv: Advice, exprs: List[expr]): Advice = {
    //    val exprs = adv.exprs
    val variableNames = (exprs flatMap (_.getVariables))
    //    println(exprs)
    //    println(adv.exprs)
    //    println(variableNames)
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

    new Advice(replacements.foldRight(exprs)((repl: (String, String), es: List[expr]) => {
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