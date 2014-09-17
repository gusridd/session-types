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

    val ng = Congruence(g)

    assertEquals(2, ng.exprs.size)
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
    	  
    val ng = Congruence(g)

    assertEquals(4, ng.exprs.size)
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

    val ng = Congruence(g)
    ng.print
    assertEquals(4, ng.exprs.size)
  }

  @Test def testCongruenceNaiveWeaveSimpleTradeWithNegotiation() {
    val g = new GlobalProtocol(List(
      Message("x_0^[x_0]", "S", "B", "Item", "String", "x_1^[x_0)]"),
      ChoiceJoin("x_1^[x_0]", "x_6^[x_0]", "x_2^[x_0]"),
      Choice("x_2^[x_0]", "x_3^[x_0]", "x_4^[x_0]"),
      Message("x_3^[x_0]", "B", "C", "Offer", "Int", "x_5^[x_0]"),
      Message("x_5^[x_0]", "C", "B", "Counter", "Int", "x_6^[x_0]"),
      Indirection("x_4^[x_0]", "x_1"),
      Indirection("x_0", "x_0^[x_0]"),
      Parallel("x_1", "x_2", "x_3"),
      Message("x_2", "B", "S", "Sale", "Boolean", "x_4"),
      Message("x_3", "B", "C", "Purchase", "Boolean", "x_5"),
      ParallelJoin("x_4", "x_5", "x_6"),
      End("x_6")),"x_0")

    val ng = Congruence(g)

    assertEquals(10, ng.exprs.size)

    assertTrue(ng.exprs.contains(Message("x_0", "S", "B", "Item", "String", "x_1^[x_0)]")))
  }
}