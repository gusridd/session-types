package test.lang;

import org.junit.Assert._
import org.junit.Test;

import lang._

class Order {

  @Test  def testChoiceOrder() {
    val c1 = Choice("x_1","x_2","x_3")
    val c2 = Choice("x_1","x_3","x_2")
    
    assertEquals(c1.x_1,c2.x_1)
    assertEquals(c1.x_2,c2.x_2)
    assertEquals(c1.x_3,c2.x_3)
  }
  
  @Test  def testChoiceJoinOrder() {
    val c1 = ChoiceJoin("x_3","x_2","x_1")
    val c2 = ChoiceJoin("x_2","x_3","x_1")
    
    assertEquals(c1.x_1,c2.x_1)
    assertEquals(c1.x_2,c2.x_2)
    assertEquals(c1.x_3,c2.x_3)
  }
  
  @Test  def testParallelOrder() {
    val c1 = Parallel("x_1","x_2","x_3")
    val c2 = Parallel("x_1","x_3","x_2")
    
    assertEquals(c1.x_1,c2.x_1)
    assertEquals(c1.x_2,c2.x_2)
    assertEquals(c1.x_3,c2.x_3)
  }
  
  @Test  def testParallelJoinOrder() {
    val c1 = ParallelJoin("x_3","x_2","x_1")
    val c2 = ParallelJoin("x_2","x_3","x_1")
    
    assertEquals(c1.x_1,c2.x_1)
    assertEquals(c1.x_2,c2.x_2)
    assertEquals(c1.x_3,c2.x_3)
  }
}
