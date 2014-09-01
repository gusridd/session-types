package lang.aspect

import lang.GlobalProtocol
import lang.Message

object uniqueMsg {
  /**
   * Main method that tries to check 
   */
  def apply(a: Aspect): Boolean = {
    
    val (leftHash, rightHash) = a.adv.getHashes
    def Msg(x: String, xb: Set[String], M: Set[Message]): Boolean = {
    	leftHash(x) match {
    	  case m @ Message(x,_,_,_,_,xp) => {
    	    val Mp = M - m
    	    // The disjoint sum of sets considers the empty set
    	    Msg(xp,xb,Mp) || Msg(xp,xb,M)
     	  }
    	  case AdviceTransition(x1,x2) => {
    	    val pairs = (M.subsets map (s => (s,M--s)))
    	    
    	    // This represent all the disjoint sums in whose all the elements
    	    // of the first set are matched by some pointcut.
    	    val properties = pairs filter({
    	      case (sM,sMP) => sM forall (m => Weaver.pointcutMatchGlobal(a.pc,m))
    	    })
    	    properties exists({
    	      case (m,mp) => Msg(x2,xb,mp)
    	    })
    	  }
    	}
    }
    
    /**
     * We try to find to start the computation assuming that every message
     * could be unique as we don't know from the start which one could be.
     */
    val uniqueMessages = a.adv.exprs flatMap {
      case m@Message(_,_,_,_,_,_) => Some(m)
      case _ => None
    } map (M => Msg("x_0",Set(),Set(M)))
    
    uniqueMessages.size == 1
  }
  
  def partition(s: Set[String]): (Set[String],Set[String]) = {
    val subs = s.subsets
    subs foreach (sub => println(sub + "\t\t" + (s.--(sub)).toString))
    (Set(),Set())
  }
}