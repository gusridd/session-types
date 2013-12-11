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
  def testThreadCorrectnessWrong() {
    val reader = new FileReader("./src/lang/test/threadCorrectnessWrong.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
  }
  
  @Test
  def testThreadCorrectnessCorrect() {
    val reader1 = new FileReader("./src/lang/test/threadCorrectnessGood1.txt")
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
    reader1.close
    
    val reader2 = new FileReader("./src/lang/test/threadCorrectnessGood2.txt")
    val g2: GlobalProtocol = GlobalParser.parse(reader2)
    reader2.close
    
    val reader3 = new FileReader("./src/lang/test/AlternatingBitProtocol.txt")
    val g3: GlobalProtocol = GlobalParser.parse(reader3)
    reader3.close
  }
  
  @Test
  def testReductionFiltering() {
    val c = Choice("x_0","x_3","x_4")
    val cj = ChoiceJoin("x_3","x_4","x_5")
    val exprs : List[expr] = List(c,cj)
    val filtered = (exprs filterNot (x => x == c || x == cj))
    assert(filtered.size == 0)
    
  }


}