package lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.immutable.HashSet
import scala.collection.mutable.Map
import scala.util.matching.UnanchoredRegex
import scala.math.Ordering.String
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.LinkedHashSet

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
  def parse(reader: java.io.Reader): GlobalProtocol = {
    parseAll(global, reader).get
  }
}

sealed trait expr {
  def canonical(): String = left + " = " + right
  def substitute(s1: String, s2: String): expr
  def left: String
  def right: String
  def isEnd = false
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
    case c @ Ternary(x_1, x_2, x_3) => this.x_1 == c.x_1 && this.x_2 == c.x_2 && this.x_3 == c.x_3
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
  override def isEnd = true
}
case class Continue(x_1: String, x_2: String) extends expr {
  def substitute(s1: String, s2: String): Continue = {
    Continue(x_1.sub(s1, s2), x_2.sub(s1, s2))
  }
  def left = x_1
  def right = x_2
}

class SanityConditionException(s: String) extends Exception
class LocalChoiceException(s: String) extends Exception
class NotConnectedException(s: String) extends Exception



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