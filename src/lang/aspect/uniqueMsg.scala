package lang.aspect

import lang.GlobalProtocol
import lang.Message

object uniqueMsg {
  def apply(a: Aspect): Boolean = {
    val (leftHash, rightHash) = a.adv
    def Msg(x: String, xb: Set[String], M: Set[Message]): Boolean = {
    	leftHash(x) match {
    	  case m @ Message(x,_,_,_,_,xp) => {
    	    val Mp = M - m
    	    Msg(xp,xb,Mp)
     	  }
    	  case AdviceTransition()
    	}
    }
    
    /**
     * We try to find to start the computation assuming that every message
     * could be unique as we don't know from the start which one could be.
     */
    val uniqueMessages = g.exprs flatMap {
      case m@Message(_,_,_,_,_,_) => Some(m)
      case _ => None
    } map (M => Msg("x_0",Set(),Set(M)))
    
    uniqueMessages.size == 1
  }
}