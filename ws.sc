import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import java.io.File;
import lang.LocalProtocol._
import java.io.{FileReader => FR}

object ws {
val path = "../workspace/sessionTypes/src/lang/test/"
                                                  //> path  : String = ../workspace/sessionTypes/src/lang/test/
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt")
                                                  //> reader1  : java.io.FileReader = java.io.FileReader@f846df
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
                                                  //> g1  : lang.GlobalProtocol = List(Choice(x_0,x_1,x_2), Message(x_1,Alice,Bob,
                                                  //| Book,String,x_3), Message(x_2,Alice,Bob,Film,String,x_4), ChoiceJoin(x_3,x_4
                                                  //| ,x_5), Message(x_5,Bob,Alice,Price,Int,x_6), End(x_6))
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Send("x_0","World","Say","Hello","x_1").canonical
                                                  //> res0: String = x_0 = !(World,Say(Hello)).x_1
  LocalProtocol.End("x_1").canonical              //> res1: String = x_1 = end
  Receive("x_0","Me","Say","Hello","x_1").canonical
                                                  //> res2: String = x_0 = ?(Me,Say(Hello)).x_1
  LocalProtocol.End("x_1").canonical              //> res3: String = x_1 = end
  new File(".").getCanonicalPath()                //> res4: String = /home/gsoto/ScalaIDE
  val g: GlobalProtocol = GlobalParser.parse(new FR(path + "SPARQLv1.txt"))
                                                  //> g  : lang.GlobalProtocol = List(Choice(x_0,x_1,x_3), Message(x_1,C,S,QueryGE
                                                  //| T,,x_8), Choice(x_2,x_3,x_5), Message(x_3,C,S,QueryPOST,,x_4), Message(x_4,C
                                                  //| ,S,BodymsgPOST,,x_6), Message(x_5,C,S,BodymsgEncodedPost,,x_7), ChoiceJoin(x
                                                  //| _6,x_7,x_9), ChoiceJoin(x_8,x_9,x_10), Choice(x_10,x_11,x_12), Message(x_11,
                                                  //| S,C,Error,,x_18), Choice(x_12,x_13,x_14), Message(x_13,S,C,RespGrafo,,x_15),
                                                  //|  Message(x_14,S,C,RespDocument,,x_16), ChoiceJoin(x_15,x_16,x_17), ChoiceJoi
                                                  //| n(x_17,x_18,x_19), End(x_19))
  val wf = new WFGlobalProtocol(g.exprs)          //> lang.SanityConditionException
                                                  //| 	at lang.Sanity$.apply(Sanity.scala:38)
                                                  //| 	at lang.WFGlobalProtocol.<init>(WFGlobalProtocol.scala:10)
                                                  //| 	at ws$$anonfun$main$1.apply$mcV$sp(ws.scala:22)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at ws$.main(ws.scala:10)
                                                  //| 	at ws.main(ws.scala)
}