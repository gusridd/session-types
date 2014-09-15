package lang.aspect

import lang.GlobalProtocol
import lang.WFGlobalProtocol
import lang.WFConditionException
import lang.expr
import lang.Congruence

object AspectualLocalChoice {
  def apply(aspect: Aspect, g: GlobalProtocol): Boolean = {
    try {
      val wovenExprs = Weaver.GlobalWeaving(List(aspect), g.exprs)
      wovenExprs map {x => println(x.canonical)}
      new WFGlobalProtocol(Congruence(wovenExprs).to)
      true
    } catch {
      case e: WFConditionException => false
    } 
  }

  def apply(aspects: List[Aspect], g: GlobalProtocol): Boolean = {
    val op = (exprs:List[expr], a: Aspect) => {
      Weaver.GlobalWeaving(List(a), exprs)
    }
    val wovenExprs: List[lang.expr] = aspects.foldLeft(g.exprs)(op)
    println("------before")
    wovenExprs map {x => println(x.canonical)}
    println("------after")
    try {
      new WFGlobalProtocol(Congruence(wovenExprs).to)
      true
    } catch {
      case e: WFConditionException => false
    }
  }
}