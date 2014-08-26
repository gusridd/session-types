package lang.aspect

import lang.GlobalProtocol
import lang.expr
import lang.Message
import lang.LocalProtocol.Send

object Weaver {
	def naiveGlobalWeaving(aspects: List[Aspect], exprs : List[expr]): List[expr] = aspects match {
	  case aspect :: aRest => exprs
	  case Nil => exprs
	}
	
	/**
	 * Pointcut Matching
	 */
	private[this] def pointcutMatchGlobal(pcs: List[Pointcut], e: expr) = e match {
	  case Message(x1,s1,r1,l1,t1,x2) => pcs exists {
	    case Pointcut(s2,r2,l2,t2) => s1 == s2 && r1 == r2 && (l2 == "*" || l2 == l1) && (t2 == "*" || t2 == t1)
	  }
	  case _ => false
	}
}