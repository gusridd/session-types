package lang

import lang.LocalProtocol.Send
import lang.LocalProtocol.Receive
import scala.collection.mutable.Set

trait Local extends Global {
  
  val exprs: List[lang.expr]
  
  override def getParticipants = {
    (exprs flatMap {
      case Send(_, p, _, _, _) => Some(Set(p))
      case Receive(_, p, _, _, _) => Some(Set(p))
      case _ => Set()
    }).reduce(_ ++ _)
  }

  override def getMessageLabels: Set[String] = (exprs flatMap {
    case Send(_, _, l, _, _) => Some(l)
    case Receive(_, _, l, _, _) => Some(l)
    case _ => None
  }).to
}