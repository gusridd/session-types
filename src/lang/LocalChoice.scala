package lang

object LocalChoice {
  def apply(g: GlobalProtocol): Boolean = {
    try {
      g.exprs forall (e => e match {
        case Choice(x, xp, xpp) => Receiver(g)(xp) == Receiver(g)(xpp) && uniqueActiveSender(g, x)
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