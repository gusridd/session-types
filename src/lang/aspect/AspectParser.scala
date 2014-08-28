package lang.aspect

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import lang.GlobalProtocol
import lang.GlobalSessionParser
import lang.expr
import lang.Identifier
import lang.stringToIdentifier

/**
 * Parser for Aspectual Session Types
 */
class AspectParser extends GlobalSessionParser {

  def aspects: Parser[List[Aspect]] = rep(aspect)

  def aspect: Parser[Aspect] = "SessionAspect" ~ qualifiedName ~ "{" ~ pointcuts ~ advice ~ "}" ^^
    { case _ ~ name ~ _ ~ pc ~ adv ~ _ => new Aspect(name, pc, adv) }

  def pointcuts: Parser[List[Pointcut]] = "pointcut:" ~ repsep(pointcut, "+") ^^
    { case _ ~ pcs => pcs }

  def pointcut: Parser[Pointcut] = (poincutWithPayloadType | poincutWithoutPayloadType ) ^^ { p => p }
  
  def poincutWithPayloadType = qualifiedName ~ "->" ~ qualifiedName ~ ":" ~ qNameWildcard ~ "(" ~ qNameWildcard ~ ")" ^^
    { case p ~ _ ~ pp ~ _ ~ l ~ _ ~ t ~ _ => new Pointcut(p, pp, l, t) }
  
  def poincutWithoutPayloadType = qualifiedName ~ "->" ~ qualifiedName ~ ":" ~ qNameWildcard ^^
    { case p ~ _ ~ pp ~ _ ~ l => new Pointcut(p, pp, l, "") }

  def advice: Parser[Advice] = "advice:" ~ rep(expr | advTransition) ^^
    { case _ ~ ls => new Advice(ls) }

  def advTransition: Parser[AdviceTransition] = xid ~ "=" ~ "proceed" ~ ";" ~ xid ^^
    { case x1 ~ _ ~ _ ~ _ ~ x2 => new AdviceTransition(x1, x2) }

  def qualifiedName: Parser[String] = """[A-Za-z0-9_*-]+""".r | failure("illegal start of id") ^^ { _.toString() }

  def qNameWildcard: Parser[String] = qualifiedName | "*" ^^ { _.toString() }
}

object AspectParser extends AspectParser {
  def parse(reader: java.io.Reader): List[Aspect] = {
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

trait AspectualAST extends expr with Positional 

case class Pointcut(s: String, r: String, l: String, t: String) extends AspectualAST {
  def left = throw new Exception("left called on Pointcut")
  def right = throw new Exception("right called on Pointcut")
  def substitute(s1: String, s2: String) = this
  def getVariables = scala.collection.mutable.Set()
}

case class Advice(ls: List[expr]) extends AspectualAST {
  def left = throw new Exception("left called on Advice")
  def right = throw new Exception("right called on Advice")
  def substitute(s1: String, s2: String) = {
    new Advice(ls map (_.substitute(s1, s2)))
  }
  def getVariables = ls.flatMap(_.getVariables).to
}

case class AdviceTransition(x1: String, x2: String) extends AspectualAST {
  override def left = x1
  override def right = "proceed;" + x2
  def substitute(s1: String, s2: String) = {
    new AdviceTransition(x1.sub(s1,s2),x2.sub(s1,s2))
  }
  override def getVariables = scala.collection.mutable.Set(x1,x2)
}

/**
 * Base class for the algorithms
 */

class GlobalAspectualSessionType(g: GlobalProtocol, aspects: List[Aspect])

case class Aspect(name: String, pc: List[Pointcut], adv: Advice)