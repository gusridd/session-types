package lang

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set

trait Global {

  val exprs: List[lang.expr]
  
  private var hashCacheL: Option[HashMap[String, lang.expr]] = None
  private var hashCacheR: Option[HashMap[String, lang.expr]] = None

  def getHashes(): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
    (hashCacheL, hashCacheR) match {
      case (Some(hl), Some(hr)) => return (hl, hr)
      case _ => {
        val (leftHash, rightHash) = getHashesFromExpr(exprs)
        hashCacheL = Some(leftHash)
        hashCacheR = Some(rightHash)
        (leftHash, rightHash)
      }
    }
  }

  def getHashesFromExpr(exprs: List[expr]): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
    val leftHash = HashMap[String, expr]()
    val rightHash = HashMap[String, expr]()
    exprs foreach { x => x.addToHash(leftHash, rightHash) }
    (leftHash, rightHash)
  }
  
  def getMessageLabels: Set[String] = (exprs flatMap {
    case Message(_, _, _, l, _, _) => Some(l)
    case _ => None
  }).to
  
  def getParticipants(): scala.collection.mutable.Set[String] = {
    (exprs flatMap {
      case Message(_, s, r, _, _, _) => Some(scala.collection.mutable.Set(s, r))
      case _ => Set()
    }).reduce(_ ++ _)
  }
  
}