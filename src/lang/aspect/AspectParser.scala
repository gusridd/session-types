package lang.aspect

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import lang.GlobalProtocol

/**
 * Parser for Aspectual Session Types
 */
class AspectParser extends JavaTokenParsers {

  def globalWithAspects = 3
  
  
}

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

class GlobalAspectualSessionType(g: GlobalProtocol,aspects: List[Aspect])

class Aspect