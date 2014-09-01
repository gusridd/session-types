package test.lang.aspect

import org.junit.Assert._
import org.junit.Test
import lang.aspect.AspectParser
import java.io.{ FileReader => FR }
import java.io.{ StringReader => SR }
import test.lang.PathInfo
import lang.GlobalParser
import lang.expr


class UniqueMsg extends PathInfo {
  import lang.aspect.uniqueMsg
  
  @Test def disjointSet(){
    val sets = uniqueMsg.partition(Set("A","B","C","D"))
  }
  
}