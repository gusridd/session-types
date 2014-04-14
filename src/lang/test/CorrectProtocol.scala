package lang.test;

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import lang._
import java.io.FileReader
import java.io.StringReader;
import java.io.Reader
import java.io.File

class CorrectProtocol {

  val path = "./src/protocol/correct/"
    
  val files : Array[File] = recursiveListFiles(new File(path))

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def activeSenderFromFile(filename: String, x: String, expected: String) = {
    val reader = new FileReader(path + filename)
    val g: GlobalProtocol = GlobalParser.parse(reader)
    reader.close
    assertEquals(expected, ActiveSender(g, x))
  }

  @Test def testLocalChoice() {
    files foreach (f => {
      val reader = new FileReader(f)
      println("Testing: " + f)
      val g: GlobalProtocol = GlobalParser.parse(reader)
      reader.close
      assertTrue(LocalChoice(g))
    })
  }

}
