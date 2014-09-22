package lang

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.Set
import scala.annotation.tailrec
import scala.util.parsing.input.Positional
import lang.aspect.AdviceTransition

class GlobalProtocol(val exprs: List[expr], val x_0: String) extends Positional with Global{

  private val end: String = "end"
  val xs: HashSet[String] = HashSet() ++ exprs.flatMap(e => e.getVariables)

  override def toString(): String = exprs.toString

  def contains(x: String): Boolean = xs contains x

//  def getMessageLabels: Set[String] = (exprs flatMap {
//    case Message(_, _, _, l, _, _) => Some(l)
//    case _ => None
//  }).to

  def getMapCount(exprs: Iterable[expr]): Map[String, (Int, Int)] = {
    val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
    exprs foreach {
      case Message(x1, _, _, _, _, x2) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
      case Choice(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case ChoiceJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case Parallel(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case ParallelJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case End(x) =>
        m(x) = (m(x)._1 + 1, m(x)._2)
    }
    m
  }

//  private var hashCacheL: Option[HashMap[String, lang.expr]] = None
//  private var hashCacheR: Option[HashMap[String, lang.expr]] = None
//
//  def getHashes(): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
//    (hashCacheL, hashCacheR) match {
//      case (Some(hl), Some(hr)) => return (hl, hr)
//      case _ => {
//        val (leftHash, rightHash) = getHashesFromExpr(exprs)
//        hashCacheL = Some(leftHash)
//        hashCacheR = Some(rightHash)
//        (leftHash, rightHash)
//      }
//    }
//  }
//
//  def getHashesFromExpr(exprs: List[expr] = exprs): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
//    val leftHash = HashMap[String, expr]()
//    val rightHash = HashMap[String, expr]()
//    exprs foreach { x => x.addToHash(leftHash, rightHash) }
//    (leftHash, rightHash)
//  }

//  def getParticipants(): scala.collection.immutable.Set[String] = {
//    (exprs flatMap {
//      case Message(_, s, r, _, _, _) => Some(scala.collection.immutable.Set(s, r))
//      case _ => Set()
//    }).reduce(_ ++ _)
//  }

  def print(): Unit = exprs foreach (e => println(e.canonical))

  def canonical(tabs: Int = 0): String = {
    val sb = new StringBuilder
    exprs foreach (e => sb ++= "\n" + (("\t" * tabs) + e.canonical))
    sb ++= " in " + x_0
    sb.toString
  }
}