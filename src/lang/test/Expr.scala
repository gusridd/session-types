package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._

class Expr {

  @Test def testCompareString() {
    val e1 = "hola"
    val e2 = "hola"
    assertEquals(e1, e2)
    assertTrue(e1 == e2)
  }

  @Test def testCompareMessage() {
    val e1 : expr = Message("x_1", "Alice", "Bob", "Msg", "Type", "x_2")
    val e2 : expr = Message("x_1", "Alice", "Bob", "Msg", "Type", "x_2")
    val e3 : expr = Message("x_1", "Alice", "Bobo", "Msg", "Type", "x_2")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }
  
  @Test def testEquivalenceRelation() {
    val e1 : Any = Choice("x_1", "x_2", "x_3")
    val e2 : Any = Choice("x_1", "x_2", "x_3")
    val e3 : Any = Choice("x_1", "x_2", "x_3")
    
    assert(e1 == e1)
    assert(e1 == e2 && e2 == e1)
    assert(e1 == e2 && e2 == e3 && e1 == e3)
  }

  @Test def testCompareEnd() {
    val e1 : expr = End("x_1")
    val e2 : expr = End("x_1")
    val e3 : expr = End("x_6")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }

  @Test def testCompareChoice() {
    val e1 : expr = Choice("x_1", "x_2", "x_3")
    val e2 : expr = Choice("x_1", "x_2", "x_3")
    val e3 : expr = Choice("x_1", "x_2", "x_4")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }

  @Test def testCompareChoiceJoin() {
    val e1 : expr = ChoiceJoin("x_1", "x_2", "x_3")
    val e2 : expr = ChoiceJoin("x_1", "x_2", "x_3")
    val e3 : expr = ChoiceJoin("x_1", "x_2", "x_4")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }

  @Test def testCompareParallel() {
    val e1 : expr = Parallel("x_1", "x_2", "x_3")
    val e2 : expr = Parallel("x_1", "x_2", "x_3")
    val e3 : expr = Parallel("x_1", "x_2", "x_4")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }

  @Test def testCompareParallelJoin() {
    val e1 : expr = ParallelJoin("x_1", "x_2", "x_3")
    val e2 : expr = ParallelJoin("x_1", "x_2", "x_3")
    val e3 : expr = ParallelJoin("x_1", "x_2", "x_4")
    assertTrue(e1 == e2)
    assertTrue(e1 != e3)
  }

}
