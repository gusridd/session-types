package lang

object LocalProtocol {
  sealed trait localExpr {
    def canonical(): String = left + " = " + right
    def left: String
    def right: String
  }

  case class Send(x1: String, p: String, l: String, U: String, x2: String) extends localExpr {
    def left = x1
    def right = "!(" + p + "," + l + "(" + U + "))." + x2
  }

  case class Receive(x1: String, p: String, l: String, U: String, x2: String) extends localExpr {
    def left = x1
    def right = "?(" + p + "," + l + "(" + U + "))." + x2
  }

  case class Indirection(x1: String, x2: String) extends localExpr {
    def left = x1
    def right = x2
  }

  case class InternalChoice(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " ^ " + x3
  }

  case class ExternalChoice(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " & " + x3
  }

  case class Merge(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1 + " + " + x2
    def right = x3
  }

  case class Fork(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1
    def right = x2 + " | " + x3
  }

  case class Join(x1: String, x2: String, x3: String) extends localExpr {
    def left = x1 + " | " + x2
    def right = x3
  }

  case class End(x: String) extends localExpr {
    def left = x
    def right = "end"
  }
}

