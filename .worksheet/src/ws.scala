import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import lang.LocalProtocol._

object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(241); 
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt");System.out.println("""reader1  : java.io.FileReader = """ + $show(reader1 ));$skip(57); 
    val g1: GlobalProtocol = GlobalParser.parse(reader1);System.out.println("""g1  : lang.GlobalProtocol = """ + $show(g1 ));$skip(44); 
  println("Welcome to the Scala worksheet");$skip(55); val res$0 = 
  
  Send("x_0","World","Say","Hello","x_1").canonical;System.out.println("""res0: String = """ + $show(res$0));$skip(37); val res$1 = 
  LocalProtocol.End("x_1").canonical;System.out.println("""res1: String = """ + $show(res$1));$skip(52); val res$2 = 
  Receive("x_0","Me","Say","Hello","x_1").canonical;System.out.println("""res2: String = """ + $show(res$2));$skip(37); val res$3 = 
  LocalProtocol.End("x_1").canonical;System.out.println("""res3: String = """ + $show(res$3))}
}
