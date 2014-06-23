import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import lang.LocalProtocol._

object ws {
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt")
                                                  //> reader1  : java.io.FileReader = java.io.FileReader@1621a25
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
}