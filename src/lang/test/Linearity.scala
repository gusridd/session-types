package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

class Linearity {

  @Test def testWithNoParallel() {
    val reader1 = new FileReader("./src/lang/test/threadCorrectnessGood1.txt")
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
    reader1.close 
    assertTrue(g1.linearityCheck)
  }
  
  @Test def testSimpleParallel() {
    val reader1 = new FileReader("./src/lang/test/threadCorrectnessGood2.txt")
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
    reader1.close
    assertTrue(g1.linearityCheck)
  }

}
