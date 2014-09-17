package lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import scala.collection.immutable.HashSet
import scala.collection.mutable.Map
import scala.util.matching.UnanchoredRegex
import scala.util.matching.Regex
import scala.math.Ordering.String
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.LinkedHashSet

class GlobalSessionParser extends JavaTokenParsers {

  import scala.math.Ordering.String

  def global: Parser[GlobalProtocol] = (implicitStart | explicitStart) ^^ ({
    case (exprs,x0) => new GlobalProtocol(exprs,x0)
  })

  def explicitStart: Parser[(List[expr],String)] = (rep(expr) ~ "in" ~ xid) ^^ ({
    case x ~ _ ~ x0 => (x,x0)
  })
  
  def implicitStart: Parser[(List[expr],String)] = rep(expr) ^^ ({
    case x => (x,"x_0")
  })

  def expr: Parser[expr] = positioned(message | choice | choiceJoin | parallel | parallelJoin | end | indirection | failure("illegal start of protocol")) ^^ (x => x)

  def message: Parser[Message] = (messageWithType | messageWithoutType | failure("illegal start of message")) ^^ (x => x)

  def messageWithType: Parser[Message] = (xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ "(" ~ id ~ ")" ~ ";" ~ xid | failure("illegal start of typed message")) ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ t ~ _ ~ _ ~ x2 => Message(x1, s, r, m, t, x2)
  }

  def messageWithoutType: Parser[Message] = (xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ ";" ~ xid | failure("illegal start of simple message")) ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ x2 => Message(x1, s, r, m, "", x2)
  }

  def choice: Parser[Choice] = (xid ~ "=" ~ xid ~ "+" ~ xid | failure("illegal start of choice")) ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 =>
      Choice(x1, x2, x3)
  }

  def choiceJoin: Parser[ChoiceJoin] = (xid ~ "+" ~ xid ~ "=" ~ xid | failure("illegal start of choiceJoin")) ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 =>
      ChoiceJoin(x1, x2, x3)
  }

  def parallel: Parser[Parallel] = (xid ~ "=" ~ xid ~ "|" ~ xid | failure("illegal start of parallel")) ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 => Parallel(x1, x2, x3)
  }

  def parallelJoin: Parser[ParallelJoin] = (xid ~ "|" ~ xid ~ "=" ~ xid | failure("illegal start of parallelJoin")) ^^ {
    case x1 ~ _ ~ x2 ~ _ ~ x3 => ParallelJoin(x1, x2, x3)
  }

  def indirection: Parser[Indirection] = xid ~ "=" ~ xid ^^ {
    case x1 ~ _ ~ x2 => Indirection(x1, x2)
  }

  def end: Parser[End] = (xid ~ "=" ~ "end" | failure("illegal start of end")) ^^ { case x ~ _ ~ _ => new End(x) }

  def xid: Parser[String] = ("""x_[0-9]+""".r | failure("illegal start of xid")) ^^ { _.toString() }

  def id: Parser[String] = ("""[A-Z][A-Za-z0-9\-_]*""".r | failure("illegal start of id")) ^^ { _.toString() }

}

object GlobalParser extends GlobalSessionParser {
  def parse(reader: java.io.Reader): GlobalProtocol = {
    parseAll(global, reader) match {
      case Success(gp, _) => {
        gp
      }
      case Failure(msg, next) => {
        println("[" + next.pos + "] failure: " + msg)
        throw new Exception("Parser error: [" + next.pos + "] " + msg)
      }
      case Error(msg, next) => {
        println("[" + next.pos + "] error: " + msg)
        throw new Exception("Parser error: [" + next.pos + "] " + msg)
      }
    }
  }
}

trait expr extends Positional {
  def canonical(): String = left + " = " + right
  def substitute(s1: String, s2: String): expr
  def left: String
  def right: String
  def isEnd = false
  def getVariables: Set[String]

  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]): Unit
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
  def getVariables = Set(x_1, x_2, x_3)
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
  def getVariables = Set(x_1, x_2)

  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    rHash(x_2) = this
  }
}
class Choice private (val x_1: String, val x_2: String, val x_3: String) extends expr with Ternary {
  type T = Choice
  protected def construct(x_1: String, x_2: String, x_3: String) = Choice(x_1, x_2, x_3)
  override def toString(): String = "Choice(" + x_1 + "," + x_2 + "," + x_3 + ")"
  def left = x_1
  def right = x_2 + " + " + x_3
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    rHash(x_2) = this
    rHash(x_3) = this
  }
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
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    lHash(x_2) = this
    rHash(x_3) = this
  }
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
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    rHash(x_2) = this
    rHash(x_3) = this
  }
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
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    lHash(x_2) = this
    rHash(x_3) = this
  }
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
  def getVariables = Set(x)
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x) = this
    rHash("end") = this
  }
}
case class Indirection(x_1: String, x_2: String) extends expr {
  def substitute(s1: String, s2: String): Indirection = {
    Indirection(x_1.sub(s1, s2), x_2.sub(s1, s2))
  }
  def left = x_1
  def right = x_2
  def getVariables = Set(x_1, x_2)
  
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x_1) = this
    rHash(x_2) = this
  }
}

abstract class WFConditionException extends Exception

class SanityConditionException(s: String) extends WFConditionException
object SanityConditionException {
  def apply() = new SanityConditionException("")
  def apply(s: String) = new SanityConditionException(s)
}
class LocalChoiceConditionException(s: String) extends WFConditionException
object LocalChoiceConditionException {
  def apply() = new LocalChoiceConditionException("")
  def apply(s: String) = new LocalChoiceConditionException(s)
}
class LinearityConditionException(s: String) extends WFConditionException
object LinearityConditionException {
  def apply() = new LinearityConditionException("")
  def apply(s: String) = new LinearityConditionException(s)
}

class NotConnectedException(s: String) extends Exception

