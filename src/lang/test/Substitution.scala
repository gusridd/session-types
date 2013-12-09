package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._

class Substitution {

  @Test def testMessageSubstitution() {
    val c = Message("x_1", "Alice", "Bob", "Msg", "Type", "x_2")
    val nc1 = c.substitute("x_1", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_2")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_1")
    assertEquals(nc2.x_2, "x_2")
  }
  
  @Test def testEndSubstitution() {
    val c = End("x_10")
    val nc1 = c.substitute("x_10", "x_4")

    assertEquals(nc1.x, "x_4")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x, "x_10")
  }
  
  @Test def testContinueSubstitution() {
    val c = Continue("x_10","x_11")
    val nc1 = c.substitute("x_10", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_11")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_10")
    assertEquals(nc2.x_2, "x_11")

  }
  
  @Test def testChoiceSubstitution() {
    val c = Choice("x_1", "x_2", "x_3")
    val nc1 = c.substitute("x_1", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_2")
    assertEquals(nc1.x_3, "x_3")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_1")
    assertEquals(nc2.x_2, "x_2")
    assertEquals(nc2.x_3, "x_3")
  }

  @Test def testChoiceJoinSubstitution() {
    val c = ChoiceJoin("x_1", "x_2", "x_3")
    val nc1 = c.substitute("x_1", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_2")
    assertEquals(nc1.x_3, "x_3")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_1")
    assertEquals(nc2.x_2, "x_2")
    assertEquals(nc2.x_3, "x_3")
  }

  @Test def testParallelSubstitution() {
    val c = Parallel("x_1", "x_2", "x_3")
    val nc1 = c.substitute("x_1", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_2")
    assertEquals(nc1.x_3, "x_3")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_1")
    assertEquals(nc2.x_2, "x_2")
    assertEquals(nc2.x_3, "x_3")
  }

  @Test def testParallelJoinSubstitution() {
    val c = Parallel("x_1", "x_2", "x_3")
    val nc1 = c.substitute("x_1", "x_4")

    assertEquals(nc1.x_1, "x_4")
    assertEquals(nc1.x_2, "x_2")
    assertEquals(nc1.x_3, "x_3")

    val nc2 = c.substitute("x_4", "x_2")
    assertEquals(nc2.x_1, "x_1")
    assertEquals(nc2.x_2, "x_2")
    assertEquals(nc2.x_3, "x_3")
  }

}
