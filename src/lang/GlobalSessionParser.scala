package lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.immutable.HashSet
import scala.collection.mutable.Map
import scala.util.matching.UnanchoredRegex

class GlobalSessionParser extends JavaTokenParsers {

  def global: Parser[GlobalProtocol] = rep(expr) ^^ (x => new GlobalProtocol(x))

  def expr: Parser[expr] = (message | choice | choiceJoin | parallel | parallelJoin | end) ^^ (x => x)

  def message: Parser[message] = (messageWithType | messageWithoutType) ^^ (x => x)

  def messageWithType: Parser[message] = xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ "(" ~ id ~ ")" ~ ";" ~ xid ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ t ~ _ ~ _ ~ x2 => new message(x1, s, r, m, t, x2)
  }

  def messageWithoutType: Parser[message] = xid ~ "=" ~ id ~ "->" ~ id ~ ":" ~ id ~ ";" ~ xid ^^ {
    case x1 ~ _ ~ s ~ _ ~ r ~ _ ~ m ~ _ ~ x2 => new message(x1, s, r, m, "", x2)
  }

  def choice: Parser[choice] = xid ~ "=" ~ xid ~ "+" ~ xid ^^ { case x1 ~ _ ~ x2 ~ _ ~ x3 => new choice(x1, x2, x3) }

  def choiceJoin: Parser[choiceJoin] = xid ~ "+" ~ xid ~ "=" ~ xid ^^ { case x1 ~ _ ~ x2 ~ _ ~ x3 => new choiceJoin(x1, x2, x3) }

  def parallel: Parser[parallel] = xid ~ "=" ~ xid ~ "|" ~ xid ^^ { case x1 ~ _ ~ x2 ~ _ ~ x3 => new parallel(x1, x2, x3) }

  def parallelJoin: Parser[parallelJoin] = xid ~ "|" ~ xid ~ "=" ~ xid ^^ { case x1 ~ _ ~ x2 ~ _ ~ x3 => new parallelJoin(x1, x2, x3) }

  def end: Parser[end] = xid ~ "=" ~ "end" ^^ { case x ~ _ ~ _ => new end(x) }

  def xid: Parser[String] = """x_[0-9]+""".r ^^ { _.toString() }

  def id: Parser[String] = """[A-Z][a-z0-9]*""".r ^^ { _.toString() }

}

object GlobalParser extends GlobalSessionParser {
  def parse(reader: java.io.FileReader): GlobalProtocol = {
    parseAll(global, reader).get
  }
}

trait expr {
  def canonical(): String
  def replace(s1: String, s2: String): expr
  
  
  class replaceable(self: String) {
    def sub(s1: String, replacement: String): String = if (self == s1) replacement else s1
  }
  implicit def stringToReplaceable(s: String) = new replaceable(s)
}

case class message(x_1: String, s: String, r: String, msg: String, t: String, x_2: String) extends expr {
  def canonical(): String = x_1 + "=" + s + "->" + r + ":" + msg + "(" + t + ");" + x_2
  def replace(s1: String, s2: String): message = {
    new message(x_1.sub(s1,s2), s, r, msg, t, x_2.sub(s1,s2))
  }
}
case class choice(x_1: String, x_2: String, x_3: String) extends expr {
  def canonical(): String = x_1 + "=" + x_2 + "+" + x_3
  def replace(s1: String, s2: String): choice = {
    new choice(x_1.sub(s1,s2),x_2.sub(s1,s2), x_3.sub(s1,s2))
  }
}
case class choiceJoin(x_1: String, x_2: String, x_3: String) extends expr {
  def canonical(): String = x_1 + "+" + x_2 + "=" + x_3
  def replace(s1: String, s2: String): choiceJoin = {
    new choiceJoin(x_1.sub(s1,s2),x_2.sub(s1,s2), x_3.sub(s1,s2))
  }
}
case class parallel(x_1: String, x_2: String, x_3: String) extends expr {
  def canonical(): String = x_1 + "=" + x_2 + "|" + x_3
  def replace(s1: String, s2: String): parallel = {
    new parallel(x_1.sub(s1,s2),x_2.sub(s1,s2), x_3.sub(s1,s2))
  }
}
case class parallelJoin(x_1: String, x_2: String, x_3: String) extends expr {
  def canonical(): String = x_1 + "|" + x_2 + "=" + x_3
  def replace(s1: String, s2: String): parallelJoin = {
    new parallelJoin(x_1.sub(s1,s2),x_2.sub(s1,s2), x_3.sub(s1,s2))
  }
}
case class end(x: String) extends expr {
  def canonical(): String = x + "= end"
  def replace(s1: String, s2: String): end = {
    new end(x.sub(s1,s2))
  }
}
case class continue(x_1: String, x_2: String) extends expr {
  def canonical(): String = x_1 + " = " + x_2
  def replace(s1: String, s2: String): continue = {
    new continue(x_1.sub(s1,s2),x_2.sub(s1,s2))
  }
}

class SanityConditionException(s: String) extends Exception

class GlobalProtocol(val exprs: List[expr]) {

  private val x0: String = "x_0"
  private val xs: HashSet[String] = HashSet() ++ Collector.collectStateVariables(this)

  print("sanityCheck: ")
  sanityCheck()
  println("OK!")

  override def toString(): String = exprs.toString

  def contains(x: String): Boolean = xs contains x

  private[this] def sanityCheck() = {
    val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
    var endCount = 0
    exprs foreach {
      case message(x1, _, _, _, _, x2) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
      case choice(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case choiceJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case parallel(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case parallelJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case end(x) =>
        m(x) = (m(x)._1 + 1, m(x)._2)
        endCount = endCount + 1
    }
    if (endCount > 1) throw new SanityConditionException("UniqueEnd: end appears more than once")

    val unambiguous = m filter {
      case (k, (v1, v2)) if k != x0 => v1 != 1 || v2 != 1
      case _ => false
    }
    println(unambiguous)

    if (!unambiguous.isEmpty) {
      throw new SanityConditionException("Unanbiguity: ambiguous definition at " + unambiguous.head._1)
    }

    if (m(x0)._1 != 1 && m(x0)._2 != 0) {
      throw new SanityConditionException("Unique start: x_0 must appear exactly once, on the left-hand side")
    }
  }

}

object Collector {

  def collectMessages(g: GlobalProtocol): List[String] = g.exprs filter (_ match {
    case message(_, _, _, _, _, _) => true
    case _ => false
  }) map ({ case message(_, _, _, msg, _, _) => msg }) sorted

  def collectStateVariables(g: GlobalProtocol): List[String] = (g.exprs flatMap (_ match {
    case message(x1, _, _, _, _, x2) => List(x1, x2)
    case choice(x1, x2, x3) => List(x1, x2, x3)
    case choiceJoin(x1, x2, x3) => List(x1, x2, x3)
    case parallel(x1, x2, x3) => List(x1, x2, x3)
    case parallelJoin(x1, x2, x3) => List(x1, x2, x3)
    case end(x) => List()
    case _ => List()
  }) distinct) sorted

}

object Rcv {

  def apply(g: GlobalProtocol)(x: String) = {
    r(g, "_", "_", x)
  }

  private def r(g: GlobalProtocol, xt: String, pt: String, xi: String): Set[(String, String, String)] = {
    val it = g.exprs.iterator
    //println("Rcv(G," + xt+"," + pt + ")(" + xi + ")")
    while (it.hasNext) {
      val e = it.next
      e match {
        case message(x, p, pp, _, _, xp) if (x == xi && (pt contains (pp + "·"))) => return r(g, xt, pt, xp)
        case parallelJoin(x, xpp, xp) if (x == xi || xpp == xi) => return r(g, xt, pt, xp)

        case message(x, p, pp, l, _, xp) if (x == xi && !(pt contains (pp + "·"))) => return Set((pp, l, xt)) ++ r(g, xt, pp + "·" + pt, xp)

        case choice(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)
        case parallel(x, xp, xpp) if (x == xi) => return r(g, xt, pt, xp) ++ r(g, xt, pt, xpp)

        case choiceJoin(x, xp, xpp) if ((x == xi || xp == xi) && (xt contains ("·" + xpp))) => return Set.empty
        case end(x) if (x == xi) => return Set.empty

        case choiceJoin(xp, x, xpp) if (x == xi || xp == xi) && !(xt contains ("·" + xpp)) => return r(g, xt + "·" + xpp, pt, xpp)

        case _ => Set.empty
      }
    }
    throw new Exception("Undefined Rcv(G," + xt + "," + pt + ")(" + xi + ")")

  }
}

object Lin {
  def apply(g: GlobalProtocol)(x: String) = {
    l(g, "", "", x)
  }

  private def l(g: GlobalProtocol, xt: String, pt: String, xi: String): Set[(String, String, String)] = {
    val it = g.exprs.iterator
    while (it.hasNext) {
      val e = it.next
      e match {
        case choiceJoin(x, xp, xpp) if ((x == xi) && (xpp contains xt)) => return Set.empty
        case end(x) if (x == xi) => return Set.empty

        case message(x, p, pp, label, _, xp) if (x == xi && !(pt contains pp)) => return Set((p + "·" + pp, label, xt)) ++ l(g, xt, pt, xp)

        case choice(x, xp, xpp) if (x == xi) => return l(g, xt, pt, xp) ++ l(g, xt, pt, xpp)

        case parallel(x, xp, xpp) if (x == xi) => return l(g, xt, pt, xp) ++ l(g, xt, pt, xpp)

        case choiceJoin(x, xp, xpp) if ((x == xi || xp == xi) && !(xt contains ("·" + xp))) => return l(g, xt + "·" + xpp, pt, xpp)

        case parallelJoin(x, xp, xpp) if (x == xi || xp == xi) => return l(g, xt, xp + "·" + xpp, xpp)

        case _ => throw new Exception("Undefined Lin for " + e.canonical)
      }
    }
    Set.empty
  }
}