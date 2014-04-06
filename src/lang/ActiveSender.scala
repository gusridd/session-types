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
  class NonUniqueActiveSenderException(x: String, s: Set[String]) extends Exception {
    override def toString: String = 
      "NonUniqueActiveSenderException: Active Sender for " + x + " is non singleton " + s
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
        (p => (p,reduce(g, x1, Set(), Set(p), x2, Set(), Set(p)))) filter (e => e._2)
        if(activeSenders.size > 1){
          throw new NonUniqueActiveSenderException(x,activeSenders.map(el => el._1))
        } else if (activeSenders.size == 0){
          throw new NoActiveSenders(x)
        } else {
          activeSenders.last._1
        }
      case _ => throw new NonChoiceException(x)
    }

  }

  def reduce(g: GlobalProtocol, actL: String, setL: Set[String],
    parL: Set[String], actR: String, setR: Set[String],
    parR: Set[String])(implicit justConmuted : Boolean) : Boolean = {
    val (leftHash, rightHash) = g.getHashes
    leftHash(actL) match {
      case End(x) if actL == actR => true
      case End(x) if setR.contains(actR) => true
      case ChoiceJoin(x1,x2,x) if x1 == actL && x2 == actR => true
      case _ if (setL.contains(actL) && setR.contains(actR)) => true
      case ParallelJoin(x1,x2,xp) if (actL == x1 || actL == x2) 
        && reduce(g,xp,setL,parL,actR,setR,parR)(false) => true
      case ChoiceJoin(x1,x2,xp) if (actL == x1 || actR == x2)
        && reduce(g,xp,setL,parL,actR,setR,parR)(false) => true
      case Parallel(x1,x,xp) 
      if reduce(g,x,setL+x1,parL,actR,setR,parR)(false) 
        && reduce(g,xp,setL+x1,parL,actR,setR,parR)(false) => true
      case Choice(x1,x,xp) if reduce(g,x,setL+x1,parL,actR,setR,parR)(false)
        && reduce(g,xp,setL+x1,parL,actR,setR,parR)(false) => true
      case Message(x1,p,pp,_,_,x1p) if parL.contains(p) 
        && reduce(g,x1p,setL+x1,parL+pp,actR,setR,parR)(false) => true
      case _ => {
        if(justConmuted) false
        else {
          reduce(g, actR, setR, parR, actL,setL,parL)(true)
        }
      }
    }
  }
}