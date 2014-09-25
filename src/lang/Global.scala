package lang

import scala.collection.mutable.HashMap
import lang.LocalProtocol.localExpr
import lang.LocalProtocol.Send
import lang.LocalProtocol.Receive

trait Global {
  val exprs: List[expr]
  val x_0: String
  type iMap[K, V] = scala.collection.immutable.Map[K, V]

  lazy val (lHash, rHash) = getHashesFromExpr(exprs)

  def getHashes(): (iMap[String, lang.expr], iMap[String, lang.expr]) = {
    (lHash, rHash)
  }

  def getHashesFromExpr(exprs: List[expr]): (iMap[String, expr], iMap[String, expr]) = {
    val leftHash = HashMap[String, expr]()
    val rightHash = HashMap[String, expr]()
    exprs foreach { x => x.addToHash(leftHash, rightHash) }
    val l = collection.immutable.HashMap[String, lang.expr]() ++ leftHash
    val r = collection.immutable.HashMap[String, lang.expr]() ++ rightHash
    (l, r)
  }

  def getParticipants: Set[String] = {
    ((exprs flatMap {
      case e @ Message(_, _, _, _, _, _) => Some(e)
      case _ => None
    }) flatMap {
      case Message(_, s, r, _, _, _) => Set(s, r)
    }).toSet
  }

  def getMessageLabels: Set[String] = (exprs flatMap {
    case Message(_, _, _, l, _, _) => Some(l)
    case _ => None
  }).to
  
  def canonical(tabs: Int = 0): String = {
    val sb = new StringBuilder
    exprs foreach (e => sb ++= "\n" + (("\t" * tabs) + e.canonical))
    sb ++= " in " + x_0
    sb.toString
  }
}

trait Local extends Global {
  override val exprs: List[localExpr]
  
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