package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr
import lang.Message
import lang.aspect.Aspect
import lang.WFGlobalProtocol

class UniqueMsg extends PathInfo {
  import lang.aspect.uniqueMsg

  val testSet = Set(
    Message("", "A", "", "", "", ""),
    Message("", "B", "", "", "", ""),
    Message("", "C", "", "", "", ""),
    Message("", "D", "", "", "", ""))

  @Test def testDisjointPartition() {
    val sets = uniqueMsg.disjointPartition(testSet)
    sets foreach {
      case (s1, s2) => {
        // The sum of the subsets should be the initial set
        assertEquals(testSet, s1 ++ s2)
        // The set intersection should be the empty set
        assertEquals(Set(), s1.intersect(s2))
      }
    }
  }

  @Test def testDisjointPartitionSize() {
    val sets = uniqueMsg.disjointPartition(testSet)
    assertEquals(16, sets.size)
  }

  @Test def testSumPartition() {
    val sets = uniqueMsg.sumPartition(testSet)

    sets foreach {
      case (s1, s2) => {
        // The sum of the subsets should be the initial set
        assertEquals(testSet, s1 ++ s2)
      }
    }
  }

  @Test def testSumPartitionSize() {
    val sets = uniqueMsg.sumPartition(testSet)
    assertEquals(81, sets.size)
  }

  @Test def testUniqueMsgLogging() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertTrue(uniqueMsg(protocol, a))
    }
  }
  
  @Test def testMFUniqueMsgLogging() {
    val aspects = AspectParser.parse(new FR(path_mf_a + "Logging.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertFalse(uniqueMsg(protocol, a))
    }
  }

  @Test def testUniqueMsgNegotiation() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertTrue(uniqueMsg(protocol, a))
    }
  }
  
  /**
   * TODO: Enhance the uniqueMsg funcion so this passes.
   * This one is tricky as in the paper says it should pass, but it shall not
   */
  @Test def testUniqueMsgAuthentication() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertFalse(uniqueMsg(protocol, a))
    }
  }
  
  @Test def testUniqueMsgFarm() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Farm.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertTrue(uniqueMsg(protocol, a))
    }
  }
  
  @Test def testUniqueMsgGather() {
    val aspects = AspectParser.parse(new FR(path_wf_a + "Gather.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "NonParallelized.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertTrue(uniqueMsg(protocol, a))
    }
  }
  
  @Test def testMFUniqueMsgLoggingTwice() {
    val aspects = AspectParser.parse(new FR(path_mb_a + "LoggingTwice.txt"))
    val protocol = WFGlobalProtocol(GlobalParser.parse(new FR(path_wf + "SimpleTrade.txt")))

    assertEquals(1, aspects.size)

    aspects foreach {
      case a: Aspect => assertFalse(uniqueMsg(protocol, a))
    }
  }

}