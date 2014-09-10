package test.lang

import org.junit.Assert._
import org.junit.Test
import lang._
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import java.io.Reader

class Congruence {

  @Test def testCongruenceIndirection() {
    val g = GlobalParser.parse(new SR(
      """ x_0 = A -> B: Hi; x_1
	      x_1 = x_2
	      x_2 = x_3
	      x_3 = end
    	  """))
    val reduced = Congruence(g)
    val ng = new GlobalProtocol(reduced.to)
    ng.print
    assertEquals(2,ng.exprs.size)
  }

  @Test def testCongruenceIndirectionWithParallel() {
    val g = GlobalParser.parse(new SR(
      """ x_0 = A -> B: Hi; x_1
	      x_1 = x_2
	      x_2 = x_3
	      x_3 = x_4 | x_5
          x_5 = x_7
	x_4 | x_7 = x_6
	      x_6 = end
    	  """))
    val reduced = Congruence(g)
    val ng = new GlobalProtocol(reduced.to)
    ng.print
    assertEquals(4,ng.exprs.size)
  }

  @Test def testCongruenceIndirectionWithChoice() {
    val g = GlobalParser.parse(new SR(
      """ x_0 = A -> B: Hi; x_1
	      x_1 = x_2
	      x_2 = x_3
	      x_3 = x_4 + x_5
          x_5 = x_7
    x_4 + x_7 = x_6
	      x_6 = end
    	  """))
    val reduced = Congruence(g)
    val ng = new GlobalProtocol(reduced.to)
    ng.print
    assertEquals(4,ng.exprs.size)
  }
}