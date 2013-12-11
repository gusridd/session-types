import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(213); 
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt");System.out.println("""reader1  : java.io.FileReader = """ + $show(reader1 ));$skip(57); 
    val g1: GlobalProtocol = GlobalParser.parse(reader1);System.out.println("""g1  : lang.GlobalProtocol = """ + $show(g1 ));$skip(44); 
  println("Welcome to the Scala worksheet")}
}
