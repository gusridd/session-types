package lang

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.immutable.Stack
import scala.annotation.tailrec
import scala.util.control.Breaks._

object ActiveSender {

  class NonChoiceException(x: String) extends Exception {
    override def toString: String = 
      "NonChoiceException: Active Sender is not defined for " + x
  }
  class NoActiveSenders(x: String) extends Exception {
    override def toString: String = 
      "NoActiveSenders: No active senders found for " + x
  }
  

  def apply(g: GlobalProtocol, x: String) = {
    val (leftHash, rightHash) = g.getHashes
    leftHash(x) match {
      case Choice(x1, x2, x3) =>
        /**
         * At this point we don't know which is the correct singleton active 
         * sender so we try them all.
         */
        implicit val justConmuted = false
        val activeSenders = g.getParticipants map 
        (p => (p,reduce(g, x1, Set(), Set(p), x2, Set(), Set(p),false))) filter (e => e._2)
        // Non singleton case
        if (activeSenders.size != 1){
          throw new NoActiveSenders(x)
        } else {
          // Singleton case
          activeSenders.last._1
        }
      case _ => throw new NonChoiceException(x)
    }

  }
  
  def reduce(g: GlobalProtocol, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], justConmuted : Boolean) : Boolean = {
    val (leftHash, rightHash) = g.getHashes
    
    @tailrec def tailreduce(actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], justConmuted : Boolean, 
    cRemain: Stack[(String,Set[String],Set[String],String,Set[String],Set[String],Boolean)]) : Boolean = {
      leftHash(actL) match {
        case End(x) if actL == actR => {
          if (cRemain.isEmpty) true
          else {
            val (al,sl,pl,ar,sr,pr,j) = cRemain.top
            tailreduce(al,sl,pl,ar,sr,pr,j,cRemain.pop)
          }
        }
        case End(x) if setR.contains(actR) => {
          if (cRemain.isEmpty) true
          else {
            val (al,sl,pl,ar,sr,pr,j) = cRemain.top
            tailreduce(al,sl,pl,ar,sr,pr,j,cRemain.pop)
          }
        }
        case ChoiceJoin(x1,x2,x) if x1 == actL && x2 == actR => {
          if (cRemain.isEmpty) true
          else {
            val (al,sl,pl,ar,sr,pr,j) = cRemain.top
            tailreduce(al,sl,pl,ar,sr,pr,j,cRemain.pop)
          }
        }
        case _ if (setL.contains(actL) && setR.contains(actR)) => {
          if (cRemain.isEmpty) true
          else {
            val (al,sl,pl,ar,sr,pr,j) = cRemain.top
            tailreduce(al,sl,pl,ar,sr,pr,j,cRemain.pop)
          }
        }
        case ParallelJoin(x1,x2,xp) if (actL == x1 || actL == x2) 
          => tailreduce(xp,setL,parL,actR,setR,parR,false,cRemain)
        case ChoiceJoin(x1,x2,xp) if (actL == x1 || actR == x2)
          => tailreduce(xp,setL,parL,actR,setR,parR,false,cRemain)
        case Parallel(x1,x,xp) => {
          val computation = (xp,setL+x1,parL,actR,setR,parR,false)
          tailreduce(x,setL+x1,parL,actR,setR,parR,false,cRemain.push(computation))
        }
        case Choice(x1,x,xp) => {
          val computation = (xp,setL+x1,parL,actR,setR,parR,false)
          tailreduce(x,setL+x1,parL,actR,setR,parR,false,cRemain.push(computation))  
        }
        case Message(x1,p,pp,_,_,x1p) if parL.contains(p) 
           => tailreduce(x1p,setL+x1,parL+pp,actR,setR,parR,false,cRemain)
        case _ => {
          if(justConmuted) false
          else {
            tailreduce(actR, setR, parR, actL,setL,parL,true,cRemain)
          }
        }
      }
    }
    tailreduce(actL,setL,parL,actR,setR,parR,false,Stack())
    /*leftHash(actL) match {
      case End(x) if actL == actR => true
      case End(x) if setR.contains(actR) => true
      case ChoiceJoin(x1,x2,x) if x1 == actL && x2 == actR => true
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
      case ParallelJoin(x1,x2,xp) if (actL == x1 || actL == x2) 
        => reduce(g,xp,setL,parL,actR,setR,parR,false)
      case ChoiceJoin(x1,x2,xp) if (actL == x1 || actR == x2)
        => reduce(g,xp,setL,parL,actR,setR,parR,false)
      case Parallel(x1,x,xp) 
        if reduce(g,x,setL+x1,parL,actR,setR,parR,false) 
        && reduce(g,xp,setL+x1,parL,actR,setR,parR,false) => true
      case Choice(x1,x,xp) 
        if reduce(g,x,setL+x1,parL,actR,setR,parR,false)
        && reduce(g,xp,setL+x1,parL,actR,setR,parR,false) => true
      case Message(x1,p,pp,_,_,x1p) if parL.contains(p) 
         => reduce(g,x1p,setL+x1,parL+pp,actR,setR,parR,false)
      case _ => {
        if(justConmuted) false
        else {
          reduce(g, actR, setR, parR, actL,setL,parL,true)
        }
      }
    }*/
  }
  /**
   * boolean solve(Node n) {
      put node n on the stack;
      while the stack is not empty {
        if the node at the top of the stack is a leaf {
          if it is a goal node, return true
          else pop it off the stack
        }
        else {
          if the node at the top of the stack has untried children
            push the next untried child onto the stack
          else pop the node off the stack
        }
      }
      return false
    }
   */
  def backTraceReduce(g: GlobalProtocol, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], justConmuted : Boolean) : Boolean = {
    val (leftHash, rightHash) = g.getHashes
    
    val lastRule = 9
    
    var rule = 1
    var args = (actL,setL,parL,actR,setR,parR,rule,justConmuted)
    val stack = Stack(args)
    
    val goalNode : PartialFunction[expr,Boolean] = {
      // Rule 1
      case End(x) if actL == actR => true
      // Rule 2
      case End(x) if setR.contains(actR) => true
      // Rule 3
      case ChoiceJoin(x1,x2,x) if x1 == actL && x2 == actR => true
      // Rule 4
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
    }
    
    while(!stack.isEmpty){
      var (curr_actL,curr_setL,curr_parL,curr_actR,curr_setR,curr_parR,nextRule,just) = stack.top
      // if the node at the top of the stack is a leaf
      leftHash(curr_actL) match {
        // if the node at the top of the stack is a leaf
        // if it is a goal node, return true
        case el if goalNode.isDefinedAt(el) => return true
        // else pop it off the stack
        case _ if (rule == lastRule && just) => stack.pop
        // if the node at the top of the stack is not a leaf
        case ParallelJoin(x1,x2,xp) if nextRule == 5 && (actL == x1 || actL == x2) 
            => stack.push((xp,setL,parL,actR,setR,parR,nextRule+1,false))
        case ChoiceJoin(x1,x2,xp) if nextRule == 6 && (actL == x1 || actR == x2)
        	=> stack.push((xp,setL,parL,actR,setR,parR,nextRule+1,false))
      }

    }
    false
  }
}
/**
case ChoiceJoin(x1,x2,xp) if (actL == x1 || actR == x2)
        && reduce(g,xp,setL,parL,actR,setR,parR,false) => true
**/