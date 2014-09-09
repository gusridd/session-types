package test.lang;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader


class Sanity extends PathInfo {
	
  def getProtocol(reader: Reader) = {
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g
  }
  
  def sanityCheckFile(name: String) = {
    val reader = new FR(name)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    Sanity(g)
  }

  def threadReductionFile(name: String) = {
    val reader = new FR(name)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    implicit val xs = g.xs
    Sanity.threadReduction(g.exprs)
  }

  def threadReductionString(protocol: String) = {
    val reader = new SR(protocol)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    implicit val xs = g.xs
    Sanity.threadReduction(g.exprs)
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUnambiguityLeftSide() {
    sanityCheckFile(path + "ambiguousLeftSide.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUnambiguityRightSide() {
    sanityCheckFile(path + "ambiguousRightSide.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUniqueStart() {
    sanityCheckFile(path + "uniqueStart.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUniqueEnd() {
    sanityCheckFile(path + "uniqueEnd.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testThreadCorrectnessWrong() {
    threadReductionFile(path + "threadCorrectnessWrong.txt")
  }

  @Test def testThreadCorrectnessGood1() {
    threadReductionFile(path + "threadCorrectnessGood1.txt")
  }

  @Test def testThreadCorrectnessGood2() {
    threadReductionFile(path + "threadCorrectnessGood2.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testAlternatingBitProtocol() {
    threadReductionFile(path + "AlternatingBitProtocol.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testAlternatingBitProtocol3() {
    threadReductionFile(path + "AlternatingBitProtocol3.txt")
  }

  @Test def testInterleavedChoice() {
    threadReductionFile(path + "interleavedChoice.txt")
  }

  @Test def testInterleavedSAndTSystem() {
    threadReductionFile(path + "interleavedSAndTSystem.txt")
  }

  @Test def testRecursionWithEnding() {
    threadReductionFile(path + "recursionWithEnding.txt")
  }

  @Test
  def testThreadRulesTrans() {
    threadReductionString("x_0 = A -> B : L(U); x_1")
  }

  @Test def testHelloWorld() {
    threadReductionFile(path_wf + "HelloWorld.txt")
  }

  @Test def testOnlineBookStore() {
    threadReductionFile(path_wf + "OnlineBookstore.txt")
  }

  @Test def testTravelAgency() {
    threadReductionFile(path_wf + "TravelAgency.txt")
  }

  @Test def testPostOffice() {
    threadReductionFile(path_wf + "PostOffice.txt")
  }

  @Test def testRecursiveChoice() {
    threadReductionFile(path + "RecursiveChoice.txt")
  }

  @Test
  def testThreadRulesPar() {
    threadReductionString("x_1 = x_2 | x_3 \n x_2 | x_3 = x_4")
  }

  @Test
  def testThreadRulesBra() {
    threadReductionString("x_1 = x_2 + x_3 \n x_2 + x_3 = x_4")
  }

  @Test
  def testThreadRulesRec() {
    threadReductionString("x_1 + x_2 = x_3 \n x_3 = x_4 + x_2")
    threadReductionString("x_4 + x_2 = x_3 \n x_3 = x_2 + x_1")
  }

  @Test(expected = classOf[java.util.NoSuchElementException])
  def testThreadReductionUnconnected() {
    threadReductionString("x_0 = x_1 + x_2 \n x_4 + x_5 = x_6 \n x_6 = end")
  }

  @Test
  def testReductionFiltering() {
    val c: expr = Choice("x_0", "x_3", "x_4")
    val cj: expr = ChoiceJoin("x_3", "x_4", "x_5")
    val exprs: List[expr] = List(c, cj)
    val filtered = (exprs filterNot (x => x == c || x == cj))
    assert(filtered.size == 0)

    val end: expr = End("x_6")
    val exprs2: List[expr] = List(end)
    val filtered2 = (exprs2 filterNot (x => x == End("x_6")))
    println(filtered2)
    assert(filtered2.size == 0)
  }
  
  @Test def testSanityRecursiveChoice(){
    val protocol = """
    x_1^x_0 + x_6^x_0 = x_2^x_0
    		  x_2^x_0 = x_4^x_0 + x_6^x_0
    		  x_4^x_0 = x_6
    			  x_0 = x_1^x_0
    			  x_6 = end  
    """
    val g = new GlobalProtocol(List
        (ChoiceJoin("x_1^x_0","x_6^x_0","x_2^x_0"), 
            Choice("x_2^x_0","x_4^x_0","x_6^x_0"), 
            Indirection("x_4^x_0","x_6"), 
            Indirection("x_0","x_1^x_0"), 
            End("x_6") ))
    assertTrue(Sanity(g))
  }
  
  @Test def testTradeWithLoggingAfter(){
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingAfter.txt"))
    assertTrue(Sanity(g))
  }
  
  @Test def testTradeWithLoggingBefore(){
    val g = getProtocol(new FR(path_mf + "TradeWithLoggingBefore.txt"))
    assertTrue(Sanity(g))
  }

}