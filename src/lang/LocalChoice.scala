package lang

object LocalChoice {
  def apply(g: GlobalProtocol): Boolean = {
    try {
      g.exprs forall (e => e match {
        /**
         * Notice that Receiver(g)(x) returns an Output object that handles the equality relation ==
         */
        case Choice(x, xp, xpp) => {
          println("Choice: " + Choice(x, xp, xpp))
          val rcv1 = Receiver(g)(xp)
          println("rcv1: " + rcv1)
          val rcv2 = Receiver(g)(xpp)
          println("rcv2: " + rcv2)
          val receiverEquality = rcv1 == rcv2
          println("receiverEquality: " + receiverEquality)
          val uniqueASend = uniqueActiveSender(g, x)
          println("uniqueASend: " + uniqueASend)
          receiverEquality && uniqueASend
        }
        case _ => true
      })
    } catch {
      case e: Receiver.UndefinedReceiverException => {
        System.err.println(e.toString)
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