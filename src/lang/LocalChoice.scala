package lang

object LocalChoice {
  def apply(g: GlobalProtocol): Boolean = {
    try {
      g.exprs forall (e => e match {
        /**
         * Notice that Receiver(g)(x) returns an Output object that handles the equality relation ==
         */
        case c @ Choice(x, xp, xpp) => {
          //          println("Choice: " + Choice(x, xp, xpp))
          val rcv1 = Receiver(g)(xp)
          //          println("rcv1: " + rcv1)
          val rcv2 = Receiver(g)(xpp)
          //          println("rcv2: " + rcv2)
          val receiverEquality = rcv1 == rcv2
          if (receiverEquality) {
            println("[SUCCESS] receiverEquality: " + receiverEquality)
          } else {
            println("[FAIL] receiverEquality: " + receiverEquality)
          }
          println("[INFO] Rcv(G)(" + xp + "): " + rcv1)
          println("[INFO] Rcv(G)(" + xpp + "): " + rcv2)
          val uniqueASend = uniqueActiveSender(g, x)
          println("[SUCCESS] uniqueASend: " + uniqueASend)
          val r = receiverEquality && uniqueASend
          if (r) {
            println("[SUCESS] Rcv equality and unique ASend for " + c.canonical)
          } else {
            println("[FAIL] Rcv equality and unique ASend for " + c.canonical)
          }
          r
        }
        case _ => true
      })
    } catch {
      case e: Receiver.UndefinedReceiverException => {
        System.err.println(e.toString)
        println("[EXCEPTION] UndefinedReceiverException " + e.toString)
        false
      }
    }
  }

  private def uniqueActiveSender(g: GlobalProtocol, x: String): Boolean = {
    try {
      ActiveSender(g, x)
      true
    } catch {
      case e: ActiveSender.NonChoiceException => {
        System.err.println(e.toString)
        false
      }
      case e: ActiveSender.NoActiveSenders => {
        System.err.println(e.toString)
        false
      }
    }
  }
}