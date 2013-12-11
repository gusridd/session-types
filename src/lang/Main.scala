package lang

import java.io.FileReader;

object Main extends App{
	val reader1 = new FileReader("/home/gsoto/workspace/sessionTypes/src/lang/test/threadCorrectnessGood1.txt")
    val g1: GlobalProtocol = GlobalParser.parse(reader1)
}