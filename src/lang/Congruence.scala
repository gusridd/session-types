package lang

import lang.LocalProtocol.localExpr
import scala.annotation.tailrec

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

  def apply(g: GlobalProtocol): Iterable[lang.expr] = {
    this.apply(g.exprs)
  }

  def apply(exprs: List[lang.expr]): List[lang.expr] = {
    //    exprs.foldLeft(exprs)((old: Iterable[expr], curr: expr) => curr match {
    ////      case i @ LocalProtocol.Indirection("x_0", x2) => {
    ////        (old filterNot (_ == i)).map(_.substitute(x2, "x_0"))
    ////      }
    ////      case i @ Indirection("x_0", x2) => {
    ////        (old filterNot (_ == i)).map(_.substitute(x2, "x_0"))
    ////      }
    //      case i @ LocalProtocol.Indirection(x1, x2) => {
    ////        println(curr)
    //        (old filterNot (_ == i)).map(_.substitute(x2, x1))
    //      }
    //      case i @ Indirection(x1, x2) => {
    //        println(curr)
    //        println(old filterNot (_ == i))
    //        println((old filterNot (_ == i)).map(_.substitute(x2, x1)))
    //        (old filterNot (_ == i)).map(_.substitute(x2, x1))
    //      }
    //      case i @ LocalProtocol.NullAction(x1, x2) => {
    ////        println(curr)
    //        (old filterNot (_ == i)).map(_.substitute(x2, x1))
    //      }
    //      case _ => {
    //        println(curr)
    //        old
    //      }
    //    })
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

  def apply(exprs: Iterable[localExpr]): Iterable[localExpr] = {
    exprs
  }

}