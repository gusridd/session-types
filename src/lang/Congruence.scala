package lang

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

  def apply(exprs: Iterable[lang.expr]): Iterable[lang.expr] = {
    exprs.foldLeft(exprs)((old: Iterable[expr], curr: expr) => curr match {
      case i @ LocalProtocol.Indirection("x_0", x2) => {
        (old filterNot (_ == i)).map(_.substitute(x2, "x_0"))
      }
      case i @ Indirection("x_0", x2) => {
        (old filterNot (_ == i)).map(_.substitute(x2, "x_0"))
      }
      case i @ LocalProtocol.Indirection(x1, x2) => {
        (old filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case i @ Indirection(x1, x2) => {
        (old filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case i @ LocalProtocol.NullAction(x1, x2) => {
        (old filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case _ => old
    })
  }

}