package lang

class WFGlobalProtocol(exprs: List[expr]) extends GlobalProtocol(exprs) {
  /**
   * Any Well-formed Global Protocol must comply with the sanity, local
   * choice and linearity conditions.
   */
  Sanity(this)
  LocalChoice(this)
  Linearity(this)
  
  
  
}