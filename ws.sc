import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

object ws {
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt")
                                                  //> reader1  : java.io.FileReader = java.io.FileReader@a49d4b5
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
                                                  //> [1.7] failure: string matching regex `\z' expected but `x' found
                                                  //| [1.7] failure:string matching regex `\z' expected but `x' found
                                                  //| g1  : lang.GlobalProtocol = List()
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}