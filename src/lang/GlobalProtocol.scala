package lang

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashSet
//import scala.collection.mutable.Set
import scala.collection.immutable.Set
import scala.annotation.tailrec
import scala.util.parsing.input.Positional
import lang.aspect.AdviceTransition

class GlobalProtocol(val exprs: List[expr], val x_0: String) extends Positional with Global {

  private val end: String = "end"
  val xs: HashSet[String] = HashSet() ++ exprs.flatMap(e => e.getVariables)

  override def toString(): String = exprs.toString

  def contains(x: String): Boolean = xs contains x


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

  def print(): Unit = exprs foreach (e => println(e.canonical))
  
}