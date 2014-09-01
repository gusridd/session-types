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


class UniqueMsg extends PathInfo {
  import lang.aspect.uniqueMsg
  
  @Test def disjointPartition(){
    val set = Set(
        Message("","A","","","",""),
        Message("","B","","","",""),
        Message("","C","","","",""),
        Message("","D","","","",""))
    val sets = uniqueMsg.disjointPartition(set)
    
    sets foreach {
      case (s1,s2) => {
        println(s1.toString + " U " + s2.toString)
        // The sum of the subsets should be the initial set
        assertEquals(set, s1 ++ s2)
        // The set intersection should be the empty set
        assertEquals(Set(),s1.intersect(s1))
      }
    }
  }
  
  @Test def sumPartition() {
    val set = Set(
        Message("","A","","","",""),
        Message("","B","","","",""),
        Message("","C","","","",""),
        Message("","D","","","",""))
    
    val sets = uniqueMsg.sumPartition(set)
    sets foreach {
      case (s1,s2) => {
//        println(s1.toString + " U " + s2.toString)
        // The sum of the subsets should be the initial set
        assertEquals(set, s1 ++ s2)
      }
    }
  }
  
}