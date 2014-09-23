package lang.aspect

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import lang.GlobalProtocol
import lang.GlobalSessionParser
import lang.expr
import lang.Identifier
import lang.stringToIdentifier
import scala.collection.mutable.HashMap
import lang._
import scala.collection.mutable.StringBuilder
import lang.LocalProtocol.localExpr
import lang.LocalProtocol.Send
import lang.LocalProtocol.Receive

/**
 * Parser for Aspectual Session Types
 */
class AspectParser extends GlobalSessionParser {

  def aspects: Parser[List[GlobalAspect]] = rep(aspect)

  def aspect: Parser[GlobalAspect] = "SessionAspect" ~ qualifiedName ~ "{" ~ pointcuts ~ advice ~ "}" ^^
    { case _ ~ name ~ _ ~ pc ~ adv ~ _ => new GlobalAspect(name, pc, adv) }

  def pointcuts: Parser[GlobalPointcut] = "pointcut:" ~ repsep(pointcut, "+") ^^
    { case _ ~ pcs => new GlobalPointcut(pcs) }

  def pointcut: Parser[GlobalPC] = (poincutWithPayloadType | poincutWithoutPayloadType) ^^ { p => p }

  def poincutWithPayloadType = qualifiedName ~ "->" ~ qualifiedName ~ ":" ~ qNameWildcard ~ "(" ~ qNameWildcard ~ ")" ^^
    { case p ~ _ ~ pp ~ _ ~ l ~ _ ~ t ~ _ => new GlobalPC(p, pp, l, t) }

  def poincutWithoutPayloadType = qualifiedName ~ "->" ~ qualifiedName ~ ":" ~ qNameWildcard ^^
    { case p ~ _ ~ pp ~ _ ~ l => new GlobalPC(p, pp, l, "") }

  def advice: Parser[GlobalAdvice] = (explicitAdvice | implicitAdvice) ^^ { x => x }

  def implicitAdvice: Parser[GlobalAdvice] = "advice:" ~ rep(expr | advTransition) ^^
    { case _ ~ ls => new GlobalAdvice(ls, "x_0") }

  def explicitAdvice: Parser[GlobalAdvice] = "advice:" ~ rep(expr | advTransition) ~ "in" ~ xid ^^
    { case _ ~ ls ~ _ ~ x0 => new GlobalAdvice(ls, x0) }

  def advTransition: Parser[AdviceTransition] = xid ~ "=" ~ "proceed" ~ ";" ~ xid ^^
    { case x1 ~ _ ~ _ ~ _ ~ x2 => new AdviceTransition(x1, x2) }

  def qualifiedName: Parser[String] = """[A-Za-z0-9_*-]+""".r | failure("illegal start of id") ^^ { _.toString() }

  def qNameWildcard: Parser[String] = qualifiedName | "*" ^^ { _.toString() }
}

object AspectParser extends AspectParser {
  def parse(reader: java.io.Reader): List[GlobalAspect] = {
    parseAll(aspects, reader) match {
      case Success(asps, _) => {
        asps
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

/**
 * SessionAspect Name {
 * 	pointcut: A -> B: l1(t1) | A -> *: l2(t2) | A -> C: *()
 *
 *  advice:
 *
 * }
 */

/**
 * AST definitions for the AspectParser
 */

trait AspectualAST extends expr with Positional {
  def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    //throw new Exception("Add to hash is not defined for AspectualAST " + this)
  }
}

sealed abstract class Pointcut[E, S] extends AspectualAST {
  def left = throw new Exception("left called on Pointcut")
  def right = throw new Exception("right called on Pointcut")
  def substitute(s1: String, s2: String) = this
  def getVariables = scala.collection.mutable.Set()

  def doesMatch(e: E): Boolean
  def contains(e: S): Boolean
}

case class GlobalPointcut(pcs: List[GlobalPC]) extends Pointcut[expr, GlobalPC] {
  override def canonical(): String = (pcs map { _.canonical }).mkString(" + ")

  override def doesMatch(e: expr): Boolean = e match {
    case Message(x1, s1, r1, l1, t1, x2) => pcs exists {
      case GlobalPC(s2, r2, l2, t2) if (s1 == s2 && r1 == r2) =>
        l2 == "*" || (l2 == l1 && (t2 == "*" || t2 == t1))
      case _ => false
    }
    case _ => false
  }

  def getParticipants: Set[String] = (pcs flatMap { _.getParticipants }).toSet

  override def contains(e: GlobalPC): Boolean = pcs.contains(e)
}

case class GlobalPC(s: String, r: String, l: String, t: String) {
  def canonical(): String = l match {
    case "*" => s + "->" + r + ":" + l
    case _ => s + "->" + r + ":" + l + "(" + t + ")"
  }

  def getParticipants = Set(s, r)
}

case class LocalPointcut(val pcs: List[LocalPC]) extends Pointcut[localExpr, LocalPC] {
  override def canonical(): String = (pcs map { _.canonical }).mkString(" + ")

  override def doesMatch(e: localExpr): Boolean = e match {
    case Send(x1, p, l, u, x2) => pcs exists {
      case SendPC(pc_p, pc_l, pc_u) if (p == pc_p) =>
        pc_l == "*" || (l == pc_l && (pc_u == "*" || u == pc_u))
      case _ => false
    }
    case Receive(x1, p, l, u, x2) => pcs exists {
      case ReceivePC(pc_p, pc_l, pc_u) if (p == pc_p) =>
        pc_l == "*" || (l == pc_l && (pc_u == "*" || u == pc_u))
      case _ => false
    }
    case _ => false
  }

  def isNullPc: Boolean = pcs match {
    case List(NullPC()) => true
    case _ => false
  }

  override def contains(e: LocalPC): Boolean = pcs.contains(e)
}

abstract class LocalPC(val p: String, val l: String, val u: String) {
  val symbol: String
  def canonical(): String = symbol + "(" + p + ", " + l + ", " + u + ")"
}

case class SendPC(override val p: String, override val l: String, override val u: String) extends LocalPC(p, l, u) {
  val symbol = "!"
}

case class ReceivePC(override val p: String, override val l: String, override val u: String) extends LocalPC(p, l, u) {
  val symbol = "?"
}

class NullPC extends LocalPC("", "", "") {
  val symbol: String = "0"
  override def canonical(): String = "0"
}

object NullPC {
  def apply() = new NullPC
  def unapply(n: NullPC) = Some()
}

trait Advice[A <: Advice[A]] extends AspectualAST {
  A =>
  val exprs: List[expr]
  val xa: String

  def left = throw new Exception("left called on Advice")
  def right = throw new Exception("right called on Advice")

  override def substitute(s1: String, s2: String): A

  def getVariables = exprs.flatMap(_.getVariables).to

  def getParticipants: Set[String]

  def getMessageLabels: Set[String]

  def getHashes(): (scala.collection.immutable.Map[String, lang.expr], scala.collection.immutable.Map[String, lang.expr])
  def getHashesFromExpr(exprs: List[expr]): (scala.collection.immutable.Map[String, lang.expr], scala.collection.immutable.Map[String, lang.expr])

}
class GlobalAdvice(override val exprs: List[expr], val xa: String) extends Advice[GlobalAdvice] with Global {

  val x_0 = xa

  override def substitute(s1: String, s2: String) = {
    GlobalAdvice(exprs map (_.substitute(s1, s2)), xa.sub(s1, s2))
  }

}
object GlobalAdvice {
  def apply(exprs: List[expr], xa: String) = new GlobalAdvice(exprs, xa)
}

class LocalAdvice(override val exprs: List[localExpr], val xa: String) extends Advice[LocalAdvice] with Local {

  val x_0 = xa

  override def substitute(s1: String, s2: String): LocalAdvice = {
    LocalAdvice(exprs map (_.substitute(s1, s2)), xa.sub(s1, s2))
  }

}
object LocalAdvice {
  def apply(exprs: List[localExpr], xa: String) = new LocalAdvice(exprs, xa)
}

case class AdviceTransition(x1: String, x2: String) extends AspectualAST {
  def left = x1
  def right = "proceed;" + x2

  def substitute(s1: String, s2: String) = {
    new AdviceTransition(x1.sub(s1, s2), x2.sub(s1, s2))
  }
  def getVariables = scala.collection.mutable.Set(x1, x2)

  override def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
    lHash(x1) = this
    rHash(x2) = this
  }

}

/**
 * Base class for the algorithms
 */

class GlobalAspectualSessionType(g: GlobalProtocol, aspects: List[Aspect[expr, GlobalPC, GlobalAdvice]])

class Aspect[+E, S, A <: Advice[A]](val name: String, pc: Pointcut[E, S], adv: Advice[A]) {
  override def toString(): String = {
    val sb = new StringBuilder()
    sb ++= "Name: " + name + "\n"
    sb ++= "pc: "
    sb ++= pc.canonical
    sb ++= "\nadvice:"
    adv.exprs foreach (e => sb ++= ("\n\t" + e.canonical))
    sb ++= " in " + xa
    sb.toString
  }

  def xa = adv.xa
}

case class GlobalAspect(override val name: String, pc: GlobalPointcut, adv: GlobalAdvice) extends Aspect(name, pc, adv) {
  def getDaemonLabels: Set[String] = {
    val freshParticipant = adv.getParticipants mkString ("")
    val localAsp = LocalProjection(this, freshParticipant)
    localAsp.adv.getMessageLabels
  }
}

class LocalAspect(name: String, val p: String, val pc: LocalPointcut, val adv: LocalAdvice) extends Aspect(name, pc, adv)