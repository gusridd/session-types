package lang.aspect

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import lang.GlobalProtocol

/**
 * Parser for Aspectual Session Types
 */
class AspectParser extends JavaTokenParsers {

  def aspects: Parser[List[Aspect]] = rep(aspect)

  def aspect: Parser[Aspect] = "SessionAspect" ~ qualifiedName ~ "{" ~ pointcut ~ advice ~ "}" ^^
    { case _ ~ name ~ _ ~ pc ~ adv ~ _ => new Aspect(name,pc,adv) }

  def pointcut: Parser[Pointcut] = "pointcut:" ^^ { x => new Pointcut }

  def advice: Parser[Advice] = "advice:" ^^ { x => new Advice }

  def qualifiedName: Parser[String] = """[A-Za-z_-]+""".r | failure("illegal start of id") ^^ { _.toString() }
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

sealed trait aspectualAST extends Positional

class AspectAST extends aspectualAST

case class Pointcut extends aspectualAST

case class Advice extends aspectualAST

case class AdviceTransition extends aspectualAST

case class Labels extends aspectualAST

case class Sorts extends aspectualAST

/**
 * Base class for the algorithms
 */

class GlobalAspectualSessionType(g: GlobalProtocol, aspects: List[Aspect])

class Aspect(name: String, pc: Pointcut, adv: Advice)