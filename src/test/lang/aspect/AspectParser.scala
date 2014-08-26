package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }

class Parser {

  @Test def testBasisAspect() {
    AspectParser.parse(new SR(
       """SessionAspect Name {
    		pointcut: A -> B: l1(t1) | A -> *: l2(t2) | A -> C: *()
    		advice:
 
    	  }"""))
    	  
    
  }

}
