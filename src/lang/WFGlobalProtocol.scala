package lang

import LocalProtocol._

class WFGlobalProtocol(exprs: List[expr]) extends GlobalProtocol(exprs) {
  /**
   * Any Well-formed Global Protocol must comply with the sanity, local
   * choice and linearity conditions.
   */
  if(!Sanity(this)) throw SanityConditionException()
  if(!LocalChoice(this)) throw LocalChoiceConditionException()
  if(!Linearity(this)) throw LinearityConditionException()

}

object WFGlobalProtocol {
  def apply(g: GlobalProtocol): WFGlobalProtocol = {
    new WFGlobalProtocol(g.exprs)
  }
}