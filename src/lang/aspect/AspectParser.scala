package lang.aspect

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import lang.GlobalProtocol

class AspectParser extends JavaTokenParsers {

  def globalWithAspects = 3
  
  
}




/**
 * AST definitions for the AspectParser
 */


sealed trait aspectAST extends Positional


case class GlobalAspectualSessionType(g: GlobalProtocol)