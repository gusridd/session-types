package lang.aspect

import lang.WFGlobalProtocol
import lang.SanityConditionException

/**
 * This object checks the next property: Let A be an aspect ang G be a 
 * well-formed base type. A is poincut-linear wrt G iff A^G can be 
 * naively woven to a well-formed global protocol. In our implementation
 * if a WFGlobalProtocol can be instantiated, then the protocol was WF,
 * thus this function only tries to instantiate a WFGlobalProtocol object.
 */
object PointcutLinearity {
	def apply(aspect: Aspect, g: WFGlobalProtocol): Boolean = {
	  try {
	    new WFGlobalProtocol(Weaver.naiveGlobalWeaving(List(aspect), g.exprs))
	    true
	  } catch {
	    case e: SanityConditionException => false
	  }
	}
	
	def apply(aspects: List[Aspect], g: WFGlobalProtocol): Boolean = {
	  aspects forall (this.apply(_,g))
	}
}