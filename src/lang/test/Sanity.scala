package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import java.io.StringReader;

class Sanity {

  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUnambiguityLeftSide() {
    val reader = new FileReader("./src/lang/test/ambiguousLeftSide.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.sanityCheck()
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUnambiguityRightSide() {
    val reader = new FileReader("./src/lang/test/ambiguousRightSide.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.sanityCheck()
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUniqueStart() {
    val reader = new FileReader("./src/lang/test/uniqueStart.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.sanityCheck()
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testUniqueEnd() {
    val reader = new FileReader("./src/lang/test/uniqueEnd.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.sanityCheck()
  }
  
  @Test(expected = classOf[lang.SanityConditionException]) 
  def testThreadCorrectnessWrong() {
    val reader = new FileReader("./src/lang/test/threadCorrectnessWrong.txt")
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.threadReduction()
    
  }
  
  @Test
  def testThreadCorrectnessCorrect() {
    val reader1 = new FileReader("./src/lang/test/threadCorrectnessGood1.txt")
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
    reader1.close
    g1.threadReduction()
    
    
    val reader2 = new FileReader("./src/lang/test/threadCorrectnessGood2.txt")
    val g2: GlobalProtocol = GlobalParser.parse(reader2)
    reader2.close
    g2.threadReduction()
    
//    val reader3 = new FileReader("./src/lang/test/AlternatingBitProtocol.txt")
//    val g3: GlobalProtocol = GlobalParser.parse(reader3)
//    reader3.close
//    g3.threadReduction()
//    
//    val reader4 = new FileReader("./src/lang/test/AlternatingBitProtocol3.txt")
//    val g4: GlobalProtocol = GlobalParser.parse(reader4)
//    reader4.close
//    g4.threadReduction()
    
    val reader5 = new FileReader("./src/lang/test/interleavedChoice.txt")
    val g5: GlobalProtocol = GlobalParser.parse(reader5)
    reader5.close
    g5.threadReduction()
    
    val reader6 = new FileReader("./src/lang/test/interleavedSAndTSystem.txt")
    val g6: GlobalProtocol = GlobalParser.parse(reader6)
    reader6.close
    g6.threadReduction()
  }
  
  @Test
  def testThreadRulesTrans(){
    val protocol = "x_0 = A -> B : L(U); x_1"
    val reader = new StringReader(protocol)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    g.threadReduction()
  }
  
  @Test
  def testThreadRulesPar(){
    val protocol = "x_1 = x_2 | x_3 \n x_2 | x_3 = x_4"
    val reader = new StringReader(protocol)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    g.threadReduction()
  }
  
  @Test
  def testThreadRulesBra(){
    val protocol = "x_1 = x_2 + x_3 \n x_2 + x_3 = x_4"
    val reader = new StringReader(protocol)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    g.threadReduction()
  }
  
  @Test
  def testThreadRulesRec(){
    val protocol1 = "x_1 + x_2 = x_3 \n x_3 = x_4 + x_2"
    val reader1 = new StringReader(protocol1)
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
    g1.threadReduction()
    
    val protocol2 = "x_4 + x_2 = x_3 \n x_3 = x_2 + x_1"
    val reader2 = new StringReader(protocol2)
    val g2: GlobalProtocol = GlobalParser.parse(reader2)
    g2.threadReduction()
  }
  
  @Test
  def testReductionFiltering() {
    val c : expr = Choice("x_0","x_3","x_4")
    val cj : expr = ChoiceJoin("x_3","x_4","x_5")
    val exprs : List[expr] = List(c,cj)
    val filtered = (exprs filterNot (x => x == c || x == cj))
    assert(filtered.size == 0)
    
    val end : expr = End("x_6")
    val exprs2 : List[expr] = List(end)
    val filtered2 = (exprs2 filterNot (x => x == End("x_6")))
    println(filtered2)
    assert(filtered2.size == 0)
    
  }


}