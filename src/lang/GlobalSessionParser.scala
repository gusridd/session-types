package lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.immutable.HashSet
import scala.collection.mutable.Map
import scala.util.matching.UnanchoredRegex
import scala.math.Ordering.String
import scala.collection.mutable.HashMap

class GlobalSessionParser extends JavaTokenParsers {

  import scala.math.Ordering.String

  def global: Parser[GlobalProtocol] = rep(expr) ^^ (x => new GlobalProtocol(x))

  def expr: Parser[expr] = (message | choice | choiceJoin | parallel | parallelJoin | end) ^^ (x => x)

  def message: Parser[Message] = (messageWithType | messageWithoutType) ^^ (x => x)

  def messageWithType: Parser[Message] = xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ "(" ~ id ~ ")" ~ ";" ~ xid ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ t ~ _ ~ _ ~ x2 => Message(x1, s, r, m, t, x2)
  }

  def messageWithoutType: Parser[Message] = xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ ";" ~ xid ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ x2 => Message(x1, s, r, m, "", x2)
  }

  def choice: Parser[Choice] = xid ~ "=" ~ xid ~ "+" ~ xid ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 =>
      Choice(x1, x2, x3)
  }

  def choiceJoin: Parser[ChoiceJoin] = xid ~ "+" ~ xid ~ "=" ~ xid ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 =>
      ChoiceJoin(x1, x2, x3)
  }

  def parallel: Parser[Parallel] = xid ~ "=" ~ xid ~ "|" ~ xid ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 => Parallel(x1, x2, x3)
  }

  def parallelJoin: Parser[ParallelJoin] = xid ~ "|" ~ xid ~ "=" ~ xid ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 => ParallelJoin(x1, x2, x3)
  }

  def end: Parser[End] = xid ~ "=" ~ "end" ^^ { case x ~ _ ~ _ => new End(x) }

  def xid: Parser[String] = """x_[0-9]+""".r ^^ { _.toString() }

  def id: Parser[String] = """[A-Z][a-z0-9]*""".r ^^ { _.toString() }

}

/**
 * Class for treating Strings like identifiers
 */
class identifier(self: String) {
  def sub(target: String, replacement: String): String =
    if (self == target) replacement else self

  def minimum(other: String): String =
    if (self < other) self else other

  def maximum(other: String): String =
    if (self > other) self else other
}

object GlobalParser extends GlobalSessionParser {
  def parse(reader: java.io.FileReader): GlobalProtocol = {
    parseAll(global, reader).get
  }
}

sealed trait expr {
  def canonical(): String = left + " = " + right
  def substitute(s1: String, s2: String): expr
  def left: String
  def right: String
}

sealed trait Ternary {
  type T
  protected val x_1: String
  protected val x_2: String
  protected val x_3: String
  protected def construct(x1: String, x2: String, x3: String): T
  def substitute(s1: String, s2: String): T = {
    construct(x_1.sub(s1, s2), x_2.sub(s1, s2), x_3.sub(s1, s2))
  }
  override def equals(c: Any) = c match {
    case c @ Ternary(x_1, x_2, x_3) => x_1 == c.x_1 && x_2 == c.x_2 && x_3 == c.x_3
    case _ => false
  }
  override def hashCode = x_1.hashCode + x_2.hashCode + x_3.hashCode
}

object Ternary {
  def unapply(t: Ternary): Option[(String, String, String)] = Some(t.x_1, t.x_2, t.x_3)
}

case class Message(val x_1: String, val s: String, val r: String, val msg: String, val t: String, val x_2: String) extends expr {
  def substitute(s1: String, s2: String): Message = {
    Message(x_1.sub(s1, s2), s, r, msg, t, x_2.sub(s1, s2))
  }
  def left = x_1
  def right = s + " -> " + r + " : " + msg + " (" + t + "); " + x_2
}
class Choice private (val x_1: String, val x_2: String, val x_3: String) extends expr with Ternary {
  type T = Choice
  protected def construct(x_1: String, x_2: String, x_3: String) = Choice(x_1, x_2, x_3)
  override def toString(): String = "Choice(" + x_1 + "," + x_2 + "," + x_3 + ")"
  def left = x_1
  def right = x_2 + " + " + x_3
}
/**
 * Companion object and Extractor Choices with lexicographical order
 */
object Choice {
  def apply(x_1: String, x_2: String, x_3: String): Choice =
    new Choice(x_1, String.min(x_2, x_3), String.max(x_2, x_3))

  def unapply(c: Choice): Option[(String, String, String)] = Option(c.x_1, c.x_2, c.x_3)
}
class ChoiceJoin private (val x_1: String, val x_2: String, val x_3: String) extends expr with Ternary {
  type T = ChoiceJoin
  override def toString(): String = "ChoiceJoin(" + x_1 + "," + x_2 + "," + x_3 + ")"
  protected def construct(x_1: String, x_2: String, x_3: String) = ChoiceJoin(x_1, x_2, x_3)
  def left = x_1 + " + " + x_2
  def right = x_3
}
/**
 * Companion object and Extractor ChoiceJoin with lexicographical order
 */
object ChoiceJoin {
  def apply(x_1: String, x_2: String, x_3: String): ChoiceJoin =
    new ChoiceJoin(String.min(x_1, x_2), String.max(x_1, x_2), x_3)

  def unapply(c: ChoiceJoin): Option[(String, String, String)] = Option(c.x_1, c.x_2, c.x_3)
}

class Parallel private (val x_1: String, val x_2: String, val x_3: String) extends expr with Ternary {
  type T = Parallel
  override def toString(): String = "Parallel(" + x_1 + "," + x_2 + "," + x_3 + ")"
  protected def construct(x_1: String, x_2: String, x_3: String) = Parallel(x_1, x_2, x_3)
  def left = x_1
  def right = x_2 + " | " + x_3
}
object Parallel {
  def apply(x_1: String, x_2: String, x_3: String): Parallel =
    new Parallel(x_1, String.min(x_2, x_3), String.max(x_2, x_3))
  def unapply(c: Parallel): Option[(String, String, String)] = Option(c.x_1, c.x_2, c.x_3)
}

class ParallelJoin private (val x_1: String, val x_2: String, val x_3: String) extends expr with Ternary {
  type T = ParallelJoin
  override def toString(): String = "ParallelJoin(" + x_1 + "," + x_2 + "," + x_3 + ")"
  protected def construct(x_1: String, x_2: String, x_3: String) = ParallelJoin(x_1, x_2, x_3)
  def left = x_1 + " | " + x_2
  def right = x_3
}
object ParallelJoin {
  def apply(x_1: String, x_2: String, x_3: String): ParallelJoin = {
    new ParallelJoin(String.min(x_1, x_2), String.max(x_1, x_2), x_3)
  }
  def unapply(c: ParallelJoin): Option[(String, String, String)] = {
    Option(c.x_1, c.x_2, c.x_3)
  }
}

case class End(x: String) extends expr {
  def substitute(s1: String, s2: String): End = {
    End(x.sub(s1, s2))
  }
  def left = x
  def right = "end"
}
case class Continue(x_1: String, x_2: String) extends expr {
  def substitute(s1: String, s2: String): Continue = {
    Continue(x_1.sub(s1, s2), x_2.sub(s1, s2))
  }
  def left = x_1
  def right = x_2
}

class SanityConditionException(s: String) extends Exception

class GlobalProtocol(val exprs: List[expr]) {

  private val x0: String = "x_0"
  private val xs: HashSet[String] = HashSet() ++ Collector.collectStateVariables(this)

  //  sanityCheck()

  override def toString(): String = exprs.toString

  def contains(x: String): Boolean = xs contains x

  def sanityCheck() = {
    val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
    var endCount = 0
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
        endCount = endCount + 1
    }
    if (endCount > 1) throw new SanityConditionException("UniqueEnd: end appears more than once")
    //    exprs foreach { x => println(x.canonical) }
    //    println("m")
    //    println(m)
    val unambiguous = m filter {
      case (k, (v1, v2)) if k != x0 => v1 != 1 || v2 != 1
      case _ => false
    }
    //    println("unambiguous")
    //    println(unambiguous)

    if (!unambiguous.isEmpty) {
      throw new SanityConditionException("Unanbiguity: ambiguous definition at " + unambiguous.head._1)
    }

    if (m(x0)._1 != 1 && m(x0)._2 != 0) {
      throw new SanityConditionException("Unique start: x_0 must appear exactly once, on the left-hand side")
    }
  }

  def threadReduction() = {

    def getHash(exprs: List[expr]) = {
      val leftHash = HashMap[String, expr]()
      exprs foreach {
        case e => {
          leftHash += (e.left -> e)
        }
      }
      leftHash
    }

    val hash = getHash(exprs)

    def reduce(exprs: List[expr]): (List[expr], Boolean) = {
      exprs foreach {
        case m @ Message(x1, _, _, _, _, x2) =>
          return { /*println("[Trans]"); */ ((exprs filterNot (_ == m)).map(_.substitute(x1, x2)), true) }
        case p @ Parallel(x1, x2, x3) => hash.get(p.right) match {
          case Some(pj @ ParallelJoin(x2p, x3p, x4p)) =>
            return { println("[Bra]"); ((exprs filter (x => x != p && x != pj)).map(_.substitute(x1, x4p)), true) }
          case None =>
        }
        case c @ Choice(x1, x2, x3) => hash.get(c.right) match {
          case Some(cj @ ChoiceJoin(x2p, x3p, x4p)) =>
            return {
              //              println("[Bra]: " + c + " AND " + cj)
              //              println("nonfiltered " + exprs)
              //              val filtered = (exprs filter (x => c != x && x != cj))
              //              println("filtered " + filtered)
              ((exprs filter (x => x != c && x != cj)).map(_.substitute(x1, x4p)), true)
            }
          case Some(_) =>
          case None =>
        }
        case rec @ ChoiceJoin(x1, x2, x3) => hash.get(rec.right) match {
          case Some(recj @ Choice(x3p, x4p, x2p)) if (x2 == x2p && x3 == x3p) =>
            return { println("[Rec]"); ((exprs filter (x => x != rec && x != recj)).map(_.substitute(x1, x4p)), true) }
          case Some(_) =>
          case None =>
        }
        case e @ End(x) => {
          //          println("[End] " + e);
          return ((exprs filterNot (x => x == e)).map(_.substitute(e.left, e.right)), true)
        }
        case _ =>
      }
      (exprs, false)
    }

    def STReduction(exprs: List[expr]): (List[expr], Boolean) = {
      val leftHash = HashMap[String, expr]()
      val rightHash = HashMap[String, expr]()
      val flows = collection.mutable.Map() ++ ((exprs map (t => (t, 0.0))) toMap);
      
      exprs foreach {
        case m @ Message(x1, _, _, _, _, x2) => {
          leftHash(x1) = m
          rightHash(x2) = m
        }
        case e @ End(x) => {
          leftHash(x) = e
        }
        case c @  Continue(x1,x2) => {
          leftHash(x1) = c
          rightHash(x2) = c
        }
        case c @ Choice(x1, x2, x3) => {
          leftHash(x1) = c
          rightHash(x2) = c
          rightHash(x3) = c
        }
        case p @ Parallel(x1, x2, x3) => {
          leftHash(x1) = p
          rightHash(x2) = p
          rightHash(x3) = p
        }
        case cj @ ChoiceJoin(x1,x2,x3) => {
          leftHash(x1) = cj
          leftHash(x2) = cj
          rightHash(x3) = cj
        }
        case pj @ ParallelJoin(x1,x2,x3) => {
          leftHash(x1) = pj
          leftHash(x2) = pj
          rightHash(x3) = pj
        }
      }
      
      val startingFlux = 1.0
      
      def TReduction(e : expr, flux : Double, assign : HashMap[String, (Int,Int)], toEliminate : Set[expr]) : (Set[expr],Boolean) = {
        e match {
          case p @ Parallel(x1,x2,x3) => {
            val (leftSet,leftResult) = TReduction(leftHash(x2),flux/2,assign,toEliminate + p)
            val (rightSet,rightResult) = TReduction(leftHash(x3),flux/2,assign,toEliminate + p)
            (leftSet ++ rightSet,leftResult || rightResult)
          }
          case pj @ ParallelJoin(x1,x2,x3) => {
            if(flows(pj) == 0){
              flows(pj) = flux
              (toEliminate + pj,false)
            } else if (flux + flows(pj) != startingFlux){
              // Go through and join flows
              TReduction(leftHash(x3),flux + flows(pj),assign,toEliminate + pj)
            } else {
              // A T-System has been found
              (toEliminate + pj,true)
            }
          }
          case Choice(x1,x2,x3) => (Set(),false)
          case ChoiceJoin(x1,x2,x3) => (Set(), false)
          case End(x) => (Set(),false)
          case Message(x1,_,_,_,_,x2) => throw new Exception("Messages should not exist at this point")
          case Continue(x1,x2) => throw new Exception("Continues should not exist at this point")
        }
      }
      println(exprs)
      exprs foreach {
        case c @ Choice(x1,x2,x3) => {
          val (set,result) = TReduction(c,startingFlux,HashMap[String, (Int,Int)](), Set())
          if(result){
            println("FIRST AND LAST")
            println(set.head)
            println(set.last)
          }
        }
        case p @ Parallel(x1,x2,x3) => 
        case _ => // Do nothing
      }

      (exprs, false)
    }
    println("****************\nSIMPLE REDUCTION\n****************\n")
    println("reduction: " + exprs)
    exprs foreach { x => println(x.canonical) }
    var reduction = reduce(exprs)

    while (reduction._2) {
      println("reduction: " + reduction._1)
      reduction._1 foreach { x => println(x.canonical) }
      reduction = reduce(reduction._1)
    }
    
    reduction = STReduction(reduction._1)
    while (reduction._2) {
      //      println("reduction: " + reduction._1)
      reduction._1 foreach { x => println(x.canonical) }
      reduction = STReduction(reduction._1)
    }
    
    //    println("FINAL STEP: " + reduction._1)
    if (reduction._1.length > 0)
      throw new SanityConditionException("Thread correctness: unable to reduce more " + reduction._1)
    println("*******\nSUCCESS\n*******\n")
  }

  def linearityCheck(): Boolean = {
    def nonNullSuffix(s1: String, s2: String) = {
      (s1, s2) match {
        case ("", _) => false
        case (_, "") => false
      }
    }

    val linFun = Lin(this)(_)
    exprs map ({
      case Parallel(x_1, x_2, x_3) => {
        val l1 = linFun(x_2)
        val l2 = linFun(x_3)
        println("l1: " + l1)
        println("l2: " + l2)
        l1 forall {
          case (p1, l1, x1) => l2 forall {
            case (p2, l2, x2) => l1 != l2
          }
        }
        //        false
      }
      case _ => true
    }) reduce (_ && _)
  }

}

object Collector {

  //  def collectMessages(g: GlobalProtocol): List[String] = g.exprs filter (_ match {
  //    case Message(_, _, _, _, _, _) => true
  //    case _ => false
  //  }) map ({ case Message(_, _, _, msg, _, _) => msg }) sorted

  def collectStateVariables(g: GlobalProtocol): List[String] = (g.exprs flatMap (_ match {
    case Message(x1, _, _, _, _, x2) => List(x1, x2)
    case Choice(x1, x2, x3) => List(x1, x2, x3)
    case ChoiceJoin(x1, x2, x3) => List(x1, x2, x3)
    case Parallel(x1, x2, x3) => List(x1, x2, x3)
    case ParallelJoin(x1, x2, x3) => List(x1, x2, x3)
    case End(x) => List()
    case _ => List()
  }) distinct) sorted

}

object Rcv {

  def apply(g: GlobalProtocol)(x: String) = {
    r(g, "_", "_", x)
  }

  private def r(g: GlobalProtocol, xt: String, pt: String, xi: String): Set[(String, String, String)] = {
    val it = g.exprs.iterator
    //println("Rcv(G," + xt+"," + pt + ")(" + xi + ")")
    while (it.hasNext) {
      val e = it.next
      e match {
        case Message(x, p, pp, _, _, xp) if (x == xi && (pt contains (pp + "·"))) => return r(g, xt, pt, xp)
        case ParallelJoin(x, xpp, xp) if (x == xi || xpp == xi) => return r(g, xt, pt, xp)

        case Message(x, p, pp, l, _, xp) if (x == xi && !(pt contains (pp + "·"))) => return Set((pp, l, xt)) ++ r(g, xt, pp + "·" + pt, xp)

        case Choice(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)
        case Parallel(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)

        case ChoiceJoin(x, xp, xpp) if ((x == xi || xp == xi) && (xt contains ("·" + xpp))) => return Set.empty
        case End(x) if (x == xi) => return Set.empty

        case ChoiceJoin(xp, x, xpp) if (x == xi || xp == xi) && !(xt contains ("·" + xpp)) => return r(g, xt + "·" + xpp, pt, xpp)

        case _ => Set.empty
      }
    }
    throw new Exception("Undefined Rcv(G," + xt + "," + pt + ")(" + xi + ")")

  }
}

object Lin {

  val map: scala.collection.mutable.Map[String, expr] = collection.mutable.Map();

  def initializeMap(g: GlobalProtocol) = {
    val variables = Collector.collectStateVariables(g)
    g.exprs foreach {
      case m @ Message(x1, _, _, _, _, x2) => map(x1) = m
      case c @ Choice(x1, x2, x3) => map(x1) = c
      case cj @ ChoiceJoin(x1, x2, x3) => {
        map(x1) = cj
        map(x2) = cj
      }
      case p @ Parallel(x1, x2, x3) => map(x1) = p
      case pj @ ParallelJoin(x1, x2, x3) => {
        map(x1) = pj
        map(x2) = pj
      }
      case e @ End(x) => map(x) = e
      case c @ Continue(x1, x2) => map(x1) = c
    }
  }

  def apply(g: GlobalProtocol)(x: String) = {
    initializeMap(g)
    l(g, "", "", x)
  }

  private def l(g: GlobalProtocol, xm: String, xj: String, xi: String): Set[(String, String, String)] = {

    map(xi) match {
      case ChoiceJoin(x, xp, xpp) if (xpp contains xm) => return Set.empty
      case End(x) if (x == xi) => return Set.empty

      case Message(x, p, pp, label, _, xp) if !(xj contains pp) => return Set((p + "·" + pp, label, xm)) ++ l(g, xm, xj, xp)

      case Choice(x, xp, xpp) => return l(g, xm, xj, xp) ++ l(g, xm, xj, xpp)

      case Parallel(x, xp, xpp) => return l(g, xm, xj, xp) ++ l(g, xm, xj, xpp)

      case ChoiceJoin(x, xp, xpp) if !(xm contains ("·" + xp)) => return l(g, xm + "·" + xpp, xj, xpp)

      case ParallelJoin(x, xp, xpp) => return l(g, xm, xp + "·" + xpp, xpp)

      case _ => throw new Exception("Undefined Lin for " + map(xi))
    }

    Set.empty
  }
}