package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;

class Sanity {

  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUnambiguityLeftSide() {
    val reader = new FileReader("./src/lang/test/ambiguousLeftSide.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUnambiguityRightSide() {
    val reader = new FileReader("./src/lang/test/ambiguousRightSide.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUniqueStart() {
    val reader = new FileReader("./src/lang/test/uniqueStart.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUniqueEnd() {
    val reader = new FileReader("./src/lang/test/uniqueEnd.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testThreadCorrectness() {
    val reader = new FileReader("./src/lang/test/threadCorrectness.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
  }


}