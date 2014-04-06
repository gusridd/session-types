package lang.test;

import org.junit.Assert._
import org.junit.Test;

import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader

class GlobalSession {
  
  def getParticipantsFile(filename : String) = {
    val reader = new FileReader(filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    g.getParticipants
  }

  @Test def testGetParticipants() {

    var participants = getParticipantsFile("./src/lang/test/AlternatingBitProtocol.txt")
    
    assertEquals(2,participants.size)
    assertTrue(participants.contains("Alice"))
    assertTrue(participants.contains("Bob"))
    
    participants = getParticipantsFile("./src/lang/test/uniqueStart.txt")
    
    assertEquals(3,participants.size)
    assertTrue(participants.contains("Seller"))
    assertTrue(participants.contains("Broker"))
    assertTrue(participants.contains("Client"))

  }

}
