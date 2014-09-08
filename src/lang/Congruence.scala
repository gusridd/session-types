package lang

/**
 * The methods listed here do not correspond to a congruence relation, but
 * to a congruence reduction. The basis is to take a list of expr and replace
 * all Indirection and NullAction, using the substitute method provided by expr.
 */
object Congruence {

  def apply(g: GlobalProtocol): Iterable[lang.expr] = {
    this.apply(g.exprs)
  }

  def apply(exprs: Iterable[lang.expr]): Iterable[lang.expr] = {
    exprs.foldLeft(exprs)((old: Iterable[expr], curr: expr) => curr match {
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