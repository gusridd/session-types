package lang.aspect

import lang.GlobalProtocol
import lang.WFGlobalProtocol
import lang.WFConditionException
import lang.expr
import lang.Congruence

object AspectualLocalChoice {
  def apply(aspect: GlobalAspect, g: GlobalProtocol): Boolean = {
    try {
      val wovenProtocol = Weaver.GlobalWeaving(List(aspect), g)
//      wovenExprs map {x => println(x.canonical)}
      WFGlobalProtocol(Congruence(wovenProtocol))
      true
    } catch {
      case e: WFConditionException => false
    } 
  }

  def apply(aspects: List[GlobalAspect], g: GlobalProtocol): Boolean = {
    val op = (g:GlobalProtocol, a: GlobalAspect) => {
      Weaver.GlobalWeaving(List(a), g)
    }
    val wovenProtocol = aspects.foldLeft(g)(op)
    println("------woven protocol")
//    wovenExprs map {x => println(x.canonical)}
    println("------end woven protocol")
    try {
      WFGlobalProtocol(Congruence(wovenProtocol))
      true
    } catch {
      case e: WFConditionException => false
    }
  }
}