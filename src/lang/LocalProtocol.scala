package lang

import scala.collection.mutable.Set
import scala.collection.mutable.HashMap

class LocalProtocol(override val exprs: List[LocalProtocol.localExpr], p: String) extends GlobalProtocol(exprs)

object LocalProtocol {
  sealed trait localExpr extends expr {
    //    def canonical(): String = left + " = " + right
    def left: String
    def right: String
    def substitute(s1: String, s2: String): localExpr
  }

  case class Send(x1: String, p: String, l: String, U: String, x2: String) extends localExpr {
    def left = x1
    def right = "!(" + p + "," + l + "(" + U + "))." + x2
    def substitute(s1: String, s2: String): Send = Send(x1.sub(s1, s2), p, l, U, x2.sub(s1, s2))
    def getVariables = Set(x1, x2)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
    }
  }

  case class Receive(x1: String, p: String, l: String, U: String, x2: String) extends localExpr {
    def left = x1
    def right = "?(" + p + "," + l + "(" + U + "))." + x2
    def substitute(s1: String, s2: String): Receive = {
      Receive(x1.sub(s1, s2), p, l, U, x2.sub(s1, s2))
    }
    def getVariables = Set(x1, x2)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
    }
  }

  case class Indirection(x1: String, x2: String) extends localExpr {
    def left = x1
    def right = x2
    def substitute(s1: String, s2: String): Indirection = {
      Indirection(x1.sub(s1, s2), x2.sub(s1, s2))
    }
    def getVariables = Set(x1, x2)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
    }
  }

  case class InternalChoice(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " ^ " + x3
    def substitute(s1: String, s2: String): InternalChoice = {
      InternalChoice(x1.sub(s1, s2), x2.sub(s1, s2), x3.sub(s1, s2))
    }
    def getVariables = Set(x1, x2, x3)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
      rHash(x3) = this
    }
  }

  case class ExternalChoice(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " & " + x3
    def substitute(s1: String, s2: String): ExternalChoice = {
      ExternalChoice(x1.sub(s1, s2), x2.sub(s1, s2), x3.sub(s1, s2))
    }
    def getVariables = Set(x1, x2, x3)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
      rHash(x3) = this
    }
  }

  case class Merge(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1 + " + " + x2
    def right = x3
    def substitute(s1: String, s2: String): Merge = {
      Merge(x1.sub(s1, s2), x2.sub(s1, s2), x3.sub(s1, s2))
    }
    def getVariables = Set(x1, x2, x3)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      lHash(x2) = this
      rHash(x3) = this
    }
  }

  case class Fork(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " | " + x3
    def substitute(s1: String, s2: String): Fork = {
      Fork(x1.sub(s1, s2), x2.sub(s1, s2), x3.sub(s1, s2))
    }
    def getVariables = Set(x1, x2, x3)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
      rHash(x3) = this
    }
  }

  case class Join(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1 + " | " + x2
    def right = x3
    def substitute(s1: String, s2: String): Join = {
      Join(x1.sub(s1, s2), x2.sub(s1, s2), x3.sub(s1, s2))
    }
    def getVariables = Set(x1, x2, x3)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      lHash(x2) = this
      rHash(x3) = this
    }
  }

  case class End(x: String) extends localExpr {
    def left = x
    def right = "end"
    def substitute(s1: String, s2: String): End = {
      End(x.sub(s1, s2))
    }
    def getVariables = Set(x)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x) = this
      rHash("end") = this
    }
  }

  case class NullAction(x1: String, x2: String) extends localExpr {
    def left = x1
    def right = "0; " + x2
    def substitute(s1: String, s2: String): NullAction = {
      NullAction(x1.sub(s1, s2), x2.sub(s1, s2))
    }
    def getVariables = Set(x1, x2)

    def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
    }
  }

  case class AdviceTransition(x1: String, x2: String) extends localExpr {
    def left = x1
    def right = "proceed; " + x2
    def substitute(s1: String, s2: String): AdviceTransition = {
      AdviceTransition(x1.sub(s1, s2), x2.sub(s1, s2))
    }
    def getVariables = Set(x1, x2)

    override def addToHash(lHash: HashMap[String, expr], rHash: HashMap[String, expr]) = {
      lHash(x1) = this
      rHash(x2) = this
    }
  }
}

