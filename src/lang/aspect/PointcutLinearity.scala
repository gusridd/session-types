package lang.aspect

import lang.WFGlobalProtocol
import lang.SanityConditionException
import lang.LocalChoiceConditionException
import lang.LinearityConditionException
import lang.WFConditionException

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
	    val wovenExprs = Weaver.naiveGlobalWeaving(List(aspect), g.exprs)
	    wovenExprs foreach (x => println(x.canonical))
	    println("******************************************")
	    new WFGlobalProtocol(wovenExprs)
	    true
	  } catch {
	    /**
	     * If the instantiation of the object fails for wf condition reasons,
	     * then is not poincut-linear. Any other reason should be thrown.
	     */
	    case e: WFConditionException => false
	  }
	}
	
	def apply(aspects: List[Aspect], g: WFGlobalProtocol): Boolean = {
	  aspects forall (this.apply(_,g))
	}
}