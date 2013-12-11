import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

object ws {
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt")
                                                  //> reader1  : java.io.FileReader = java.io.FileReader@41fc2fb
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
                                                  //> reduction: List(Choice(x_0,x_1,x_2), Message(x_1,Alice,Bob,Book,String,x_3),
                                                  //|  Message(x_2,Alice,Bob,Film,String,x_4), ChoiceJoin(x_3,x_4,x_5), Message(x_
                                                  //| 5,Bob,Alice,Price,Int,x_6), End(x_6))
                                                  //| reduction: List(Choice(x_0,x_2,x_3), Message(x_2,Alice,Bob,Film,String,x_4),
                                                  //|  ChoiceJoin(x_3,x_4,x_5), Message(x_5,Bob,Alice,Price,Int,x_6), End(x_6))
                                                  //| reduction: List(Choice(x_0,x_3,x_4), ChoiceJoin(x_3,x_4,x_5), Message(x_5,Bo
                                                  //| b,Alice,Price,Int,x_6), End(x_6))
                                                  //| reduction: List(ChoiceJoin(x_3,x_4,x_5), Message(x_5,Bob,Alice,Price,Int,x_6
                                                  //| ), End(x_6))
                                                  //| scala.MatchError: Some(ChoiceJoin(x_3,x_4,x_5)) (of class scala.Some)
                                                  //| 	at lang.GlobalProtocol$$anonfun$reduce$1$1.apply(GlobalSessionParser.sca
                                                  //| la:279)
                                                  //| 	at lang.GlobalProtocol$$anonfun$reduce$1$1.apply(GlobalSessionParser.sca
                                                  //| la:266)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at lang.GlobalProtocol.reduce$1(GlobalSessionParser.scala:266)
                                                  //| 	at lang.GlobalProtocol.thread
                                                  //| Output exceeds cutoff limit.
  println("Welcome to the Scala worksheet")
}