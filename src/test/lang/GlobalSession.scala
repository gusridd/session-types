package test.lang;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.{FileReader => FR}
import java.io.{StringReader => SR}
import java.io.Reader

class GlobalSession extends PathInfo{
  
  def getParticipantsFile(filename : String) = {
    val reader = new FR(path + filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.getParticipants
  }

  @Test def testGetParticipants() {

    var participants = getParticipantsFile("AlternatingBitProtocol.txt")
    
    assertEquals(2,participants.size)
    assertTrue(participants.contains("Alice"))
    assertTrue(participants.contains("Bob"))
    
    participants = getParticipantsFile("uniqueStart.txt")
    
    assertEquals(3,participants.size)
    assertTrue(participants.contains("Seller"))
    assertTrue(participants.contains("Broker"))
    assertTrue(participants.contains("Client"))

  }

}
