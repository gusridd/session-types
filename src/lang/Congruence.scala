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
    //TODO: change this to a fold
    var aux = exprs
    aux foreach ({
      case i @ LocalProtocol.Indirection(x1, x2) => {
        aux = (aux filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case i @ Indirection(x1, x2) => {
        aux = (aux filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case i @ LocalProtocol.NullAction(x1, x2) => {
        aux = (aux filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case _ =>
    })
    aux
  }

}