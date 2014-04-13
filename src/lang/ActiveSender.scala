package lang

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set

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
        (p => (p,reduce(g, x1, Set(), Set(p), x2, Set(), Set(p),0))) filter (e => e._2)
        if (activeSenders.size != 1){
          throw new NoActiveSenders(x)
        } else {
          activeSenders.last._1
        }
      case _ => throw new NonChoiceException(x)
    }

  }
  def reduce(g: GlobalProtocol, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String], counter : Int) : Boolean = {
    val (leftHash, rightHash) = g.getHashes
    leftHash(actL) match {
      /* Base cases */
      case End(x) if actL == actR => true
      case End(x) if setR.contains(actR) => true
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
      case ChoiceJoin(x1,x2,x) if (x1 == actL && x2 == actR) || (x2 == actL && x1 == actR) => true
      case _ if (counter == 5) => reduce(g, actR, setR, parR, actL,setL,parL,0)
      /* Recursive cases */
      case ParallelJoin(x1,x2,xp) if reduce(g,xp,setL,parL,actR,setR,parR,counter+1) => true
      case ChoiceJoin(x1,x2,xp) if reduce(g,xp,setL,parL,actR,setR,parR,counter+1) => true
      case Parallel(x1,x,xp) 
      if reduce(g,x,setL+x1,parL,actR,setR,parR,counter+1) 
        && reduce(g,xp,setL+x1,parL,actR,setR,parR,counter+1) => true
      case Choice(x1,x,xp) if reduce(g,x,setL+x1,parL,actR,setR,parR,counter+1)
        && reduce(g,xp,setL+x1,parL,actR,setR,parR,counter+1) => true
      case Message(x1,p,pp,_,_,x1p) if parL.contains(p) 
        && reduce(g,x1p,setL+x1,parL+pp,actR,setR,parR,counter+1) => true
      case _ => {
        if(counter == 0) false
        else {
          reduce(g, actR, setR, parR, actL,setL,parL,0)
        }
      }
    }
  }
}