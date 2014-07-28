import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import java.io.File;
import lang.LocalProtocol._
import java.io.{FileReader => FR}

object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(240); 
val path = "../workspace/sessionTypes/src/lang/test/";System.out.println("""path  : String = """ + $show(path ));$skip(110); 
  val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt");System.out.println("""reader1  : java.io.FileReader = """ + $show(reader1 ));$skip(57); 
    val g1: GlobalProtocol = GlobalParser.parse(reader1);System.out.println("""g1  : lang.GlobalProtocol = """ + $show(g1 ));$skip(44); 
  println("Welcome to the Scala worksheet");$skip(55); val res$0 = 
  
  Send("x_0","World","Say","Hello","x_1").canonical;System.out.println("""res0: String = """ + $show(res$0));$skip(37); val res$1 = 
  LocalProtocol.End("x_1").canonical;System.out.println("""res1: String = """ + $show(res$1));$skip(52); val res$2 = 
  Receive("x_0","Me","Say","Hello","x_1").canonical;System.out.println("""res2: String = """ + $show(res$2));$skip(37); val res$3 = 
  LocalProtocol.End("x_1").canonical;System.out.println("""res3: String = """ + $show(res$3));$skip(35); val res$4 = 
  new File(".").getCanonicalPath();System.out.println("""res4: String = """ + $show(res$4));$skip(76); 
  val g: GlobalProtocol = GlobalParser.parse(new FR(path + "SPARQLv1.txt"));System.out.println("""g  : lang.GlobalProtocol = """ + $show(g ));$skip(41); 
  val wf = new WFGlobalProtocol(g.exprs);System.out.println("""wf  : lang.WFGlobalProtocol = """ + $show(wf ))}
}
