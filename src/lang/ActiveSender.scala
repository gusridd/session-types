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
  }
}