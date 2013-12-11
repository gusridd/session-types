package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._

class Expr {
  
    @Test def testCompareMessage() {
		val e1 = Message("x_1","Alice","Bob","Msg","Type","x_2")
		val e2 = Message("x_1","Alice","Bob","Msg","Type","x_2")
		assertEquals(e1,e2)
	}
    
    @Test def testCompareEnd() {
		val e1 = End("x_1")
		val e2 = End("x_1")
		assertEquals(e1,e2)
	}

	@Test def testCompareChoice() {
		val e1 = Choice("x_1","x_2","x_3")
		val e2 = Choice("x_1","x_2","x_3")
		assertEquals(e1,e2)
	}
	
	@Test def testCompareChoiceJoin() {
		val e1 = ChoiceJoin("x_1","x_2","x_3")
		val e2 = ChoiceJoin("x_1","x_2","x_3")
		assertEquals(e1,e2)
	}
	
	@Test def testCompareParallel() {
		val e1 = Parallel("x_1","x_2","x_3")
		val e2 = Parallel("x_1","x_2","x_3")
		assertEquals(e1,e2)
	}
	
	@Test def testCompareParallelJoin() {
		val e1 = ParallelJoin("x_1","x_2","x_3")
		val e2 = ParallelJoin("x_1","x_2","x_3")
		assertEquals(e1,e2)
	}

}
