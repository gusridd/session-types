package lang

/**
 * The methods listed here do not correspond to a congruence relation, but
 * to a congruence reduction.
 */
object Congruence {

  def apply(g: GlobalProtocol) = {
    var exprs = g.exprs
    exprs foreach ({
      case i @ LocalProtocol.Indirection(x1, x2) => {
        exprs = (exprs filterNot (_ == i)).map(_.substitute(x1, x2))
      }
      case _ =>
    })
    exprs
  }

}