package lang.aspect

import lang.GlobalProtocol
import lang.WFGlobalProtocol
import lang.WFConditionException
import lang.expr
import lang.Congruence

object AspectualLocalChoice {
  def apply(aspect: GlobalAspect, g: GlobalProtocol): Boolean = {
    try {
      val wovenExprs = Weaver.GlobalWeaving(List(aspect), g.exprs)
      wovenExprs map {x => println(x.canonical)}
      new WFGlobalProtocol(Congruence(wovenExprs).to, g.x_0)
      true
    } catch {
      case e: WFConditionException => false
    } 
  }

  def apply(aspects: List[GlobalAspect], g: GlobalProtocol): Boolean = {
    val op = (exprs:List[expr], a: GlobalAspect) => {
      Weaver.GlobalWeaving(List(a), exprs)
    }
    val wovenExprs: List[lang.expr] = aspects.foldLeft(g.exprs)(op)
    println("------woven protocol")
    wovenExprs map {x => println(x.canonical)}
    println("------end woven protocol")
    try {
      new WFGlobalProtocol(Congruence(wovenExprs).to, g.x_0)
      true
    } catch {
      case e: WFConditionException => false
    }
  }
}