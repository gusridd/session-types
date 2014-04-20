package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader;
import java.io.StringReader;

class Sanity {

  val path = "./src/lang/test/"
  val path_wf = "./src/protocol/wellformed/"

  def sanityCheckFile(name: String) = {
    val reader = new FileReader(name)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    Sanity(g)
  }

  def threadReductionFile(name: String) = {
    val reader = new FileReader(name)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    implicit val xs = g.xs
    Sanity.threadReduction(g.exprs)
  }

  def threadReductionString(protocol: String) = {
    val reader = new StringReader(protocol)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    implicit val xs = g.xs
    Sanity.threadReduction(g.exprs)
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUnambiguityLeftSide() {
    sanityCheckFile("./src/lang/test/ambiguousLeftSide.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUnambiguityRightSide() {
    sanityCheckFile("./src/lang/test/ambiguousRightSide.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUniqueStart() {
    sanityCheckFile("./src/lang/test/uniqueStart.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testUniqueEnd() {
    sanityCheckFile("./src/lang/test/uniqueEnd.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testThreadCorrectnessWrong() {
    threadReductionFile("./src/lang/test/threadCorrectnessWrong.txt")
  }

  @Test def testThreadCorrectnessGood1() {
    threadReductionFile("./src/lang/test/threadCorrectnessGood1.txt")
  }

  @Test def testThreadCorrectnessGood2() {
    threadReductionFile("./src/lang/test/threadCorrectnessGood2.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testAlternatingBitProtocol() {
    threadReductionFile("./src/lang/test/AlternatingBitProtocol.txt")
  }

  @Test(expected = classOf[lang.SanityConditionException])
  def testAlternatingBitProtocol3() {
    threadReductionFile("./src/lang/test/AlternatingBitProtocol3.txt")
  }

  @Test def testInterleavedChoice() {
    threadReductionFile("./src/lang/test/interleavedChoice.txt")
  }

  @Test def testInterleavedSAndTSystem() {
    threadReductionFile("./src/lang/test/interleavedSAndTSystem.txt")
  }

  @Test def testRecursionWithEnding() {
    threadReductionFile("./src/lang/test/recursionWithEnding.txt")
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
    threadReductionFile("./src/lang/test/RecursiveChoice.txt")
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

}