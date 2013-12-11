import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

object ws {
  val reader = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectness.txt")
                                                  //> reader  : java.io.FileReader = java.io.FileReader@4139eeda
  val g: GlobalProtocol = GlobalParser.parse(reader)
                                                  //> sanityCheck: Map()
                                                  //| scala.MatchError: Some(Choice(x_0,x_1,x_2)) (of class scala.Some)
                                                  //| 	at lang.GlobalProtocol$$anonfun$reduce$1$1.apply(GlobalSessionParser.sca
                                                  //| la:286)
                                                  //| 	at lang.GlobalProtocol$$anonfun$reduce$1$1.apply(GlobalSessionParser.sca
                                                  //| la:278)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at lang.GlobalProtocol.reduce$1(GlobalSessionParser.scala:278)
                                                  //| 	at lang.GlobalProtocol.threadReduction(GlobalSessionParser.scala:301)
                                                  //| 	at lang.GlobalProtocol.sanityCheck(GlobalSessionParser.scala:252)
                                                  //| 	at lang.GlobalProtocol.<init>(GlobalSessionParser.scala:202)
                                                  //| 	at lang.GlobalSessionParser$$anonfun$global$2.apply(GlobalSessionParser.
                                                  //| scala:14)
                                                  //| 	at lang.GlobalSessionParser$$anonfun$global$2.apply(GlobalSessionParser.
                                                  //| scala:14)
                                                  //| 	at scala.util.parsing.combinator.Parsers$Success.map(Parsers.scala:136)
                                                  //| 	at scala.util.parsing.combinator.Parsers$Success.map(Parsers.scala:135)
                                                  //| 	at scala.util.parsing.combinator.Parsers
                                                  //| Output exceeds cutoff limit.
  println("Welcome to the Scala worksheet")
}