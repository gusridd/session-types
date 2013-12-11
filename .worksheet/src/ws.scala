import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(207); 
  val reader = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectness.txt");System.out.println("""reader  : java.io.FileReader = """ + $show(reader ));$skip(53); 
  val g: GlobalProtocol = GlobalParser.parse(reader);System.out.println("""g  : lang.GlobalProtocol = """ + $show(g ));$skip(44); 
  println("Welcome to the Scala worksheet")}
}
