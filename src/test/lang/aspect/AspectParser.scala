package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo

class Parser extends PathInfo {

  @Test def testParseBasisAspect() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: 
    			A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice: 
    			x_0 = proceed; x_1
    			x_1 = end
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testParseMultipleAspects() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: 
    			A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice: 
    			x_0 = proceed; x_1
    			x_1 = end
    	  }
    	SessionAspect MyOtherBasicAspect {
    		pointcut: A -> B: l1(t1)
    		advice:
    			x_0 = proceed; x_1
    			x_1 = proceed; x_2
    			x_2 = end
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testParseAspectWithForkJoin() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: 
    			A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice: 
    			x_0 = proceed; x_1
    			x_1 = x_2 | x_3
    			x_2 | x_3 = x_4
    			x_4 = end
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testParseAspectWithChoiceMerge() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: 
    			A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice: 
    			x_0 = proceed; x_1
    			x_1 = x_2 + x_3
    			x_2 + x_3 = x_4
    			x_4 = end
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testParseAspectWithMessages() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: 
    			A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice: 
    			x_0 = proceed; x_1
    			x_1 = A -> Logger: Log(String); x_2
    			x_2 = A -> Logger: Log; x_3
    			x_3 = end
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testParseAuthentication() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Authentication.txt"))
  }
  
  @Test def testParseLogging() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Logging.txt"))
  }
  
  @Test def testParseNegotiation() {
    val parsed = AspectParser.parse(new FR(path_wf_a + "Negotiation.txt"))
  }

}
