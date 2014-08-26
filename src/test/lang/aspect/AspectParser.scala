package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }

class Parser {

  @Test def testBasisAspect() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice:
 
    	  }"""))
    	  
    println(parsed)
  }
  
  @Test def testMultipleAspects() {
    val parsed = AspectParser.parse(new SR(
       """SessionAspect MyBasicAspect {
    		pointcut: A -> B: l1(t1) + A -> *: l2(t2) + A -> C : *(t3)
    		advice:
 
    	  }
    	SessionAspect MyOtherBasicAspect {
    		pointcut: A -> B: l1(t1)
    		advice:
 
    	  }
    
    """))
    	  
    println(parsed)
  }

}
