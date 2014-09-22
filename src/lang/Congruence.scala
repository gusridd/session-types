package lang

import lang.LocalProtocol.localExpr
import scala.annotation.tailrec
import lang.aspect.Pointcut
import lang.aspect.NullPC
import lang.aspect.LocalPointcut
import lang.aspect.LocalAdvice

/**
 * The methods listed here do not correspond to a congruence relation, but
 * to a congruence reduction. The basis is to take a list of expr and replace
 * all Indirection and NullAction, using the substitute method provided by expr.
 *
 * This also deals with the fact that all protocols start with the state "x_0".
 * If a protocol starts with an Indirection the substitute rules are different
 * so the state "x_0" is preserved. Example:
 * Typically x_3 = x_5 would replace all occurrences of x_3 for x_5, but in the
 * case x_0 = x_2 the replacement is reversed so x_2 is replaced by x_0
 */
object Congruence {

  def apply(g: GlobalProtocol): GlobalProtocol = {
    new GlobalProtocol(replace(g.exprs), g.x_0)
  }

  private[this] def replace(exprs: List[lang.expr]): List[lang.expr] = {
    @tailrec
    def reduce(exprs: List[lang.expr], rec: List[lang.expr]): List[lang.expr] = {
      rec match {
        case e :: rest => e match {
          case i @ Indirection(x1, x2) => {
            reduce((exprs filterNot (_ == i)).map(_.substitute(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case i @ LocalProtocol.Indirection(x1, x2) => {
            reduce((exprs filterNot (_ == i)).map(_.substitute(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case i @ LocalProtocol.NullAction(x1, x2) => {
            reduce((exprs filterNot (_ == i)).map(_.substitute(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case _ => reduce(exprs, rest)
        }
        case Nil => exprs
      }
    }
    reduce(exprs, exprs)
  }

  def apply(lp: LocalProtocol): LocalProtocol = {
    @tailrec
    def reduce(lp: LocalProtocol, rec: List[localExpr]): LocalProtocol = {
      rec match {
        case e :: rest => e match {
          case i @ LocalProtocol.Indirection(x1, x2) => {
            val nExprs = (lp.exprs filterNot (_ == i)).map(_.substitute(x2, x1))
            reduce(new LocalProtocol(nExprs, lp.p, lp.x_0.sub(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case i @ LocalProtocol.NullAction(x1, x2) => {
            val nExprs = (lp.exprs filterNot (_ == i)).map(_.substitute(x2, x1))
            reduce(new LocalProtocol(nExprs, lp.p, lp.x_0.sub(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case _ => {
            reduce(lp, rest)
          }
        }
        case Nil => lp
      }
    }
    reduce(lp, lp.exprs)
  }

  def apply(lAdv: LocalAdvice): LocalAdvice = {
    @tailrec
    def reduce(lAdv: LocalAdvice, rec: List[localExpr]): LocalAdvice = {
      rec match {
        case e :: rest => e match {
          case i @ LocalProtocol.Indirection(x1, x2) => {
            val nExprs = (lAdv.exprs filterNot (_ == i)).map(_.substitute(x2, x1))
            reduce(new LocalAdvice(nExprs, lAdv.xa.sub(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case i @ LocalProtocol.NullAction(x1, x2) => {
            val nExprs = (lAdv.exprs filterNot (_ == i)).map(_.substitute(x2, x1))
            reduce(new LocalAdvice(nExprs, lAdv.xa.sub(x2, x1)), rest.map(_.substitute(x2, x1)))
          }
          case _ => {
            reduce(lAdv, rest)
          }
        }
        case Nil => lAdv
      }
    }
    reduce(lAdv, lAdv.exprs)
  }

  def apply(pc: LocalPointcut): LocalPointcut = {
    val filtered = pc.pcs filter ({
      case NullPC() => false
      case _ => true
    })
    if (filtered.size == 0) {
      LocalPointcut(List(NullPC()))
    } else {
      LocalPointcut(filtered)
    }
  }

}