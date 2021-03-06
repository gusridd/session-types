package lang

object Linearity {

  def apply(g: GlobalProtocol): Boolean = {
    val ng = Congruence(g)
//    val ng = new GlobalProtocol(c.to,g.x_0)
    //    g.print
    //    g.getHashes._1 foreach (e => println(e._1 + " => " + e._2.canonical))
    //    println("<---------------->")
    //    ng.print
    //    ng.getHashes._1 foreach (e => println(e._1 + " => " + e._2.canonical))
    ng.exprs forall (e => e match {
      case Parallel(x, xp, xpp) => {
        val l1 = lin(g)(xp)
        val l2 = lin(g)(xpp)
        val res = l1 == l2
        if (res) {
          println("[SUCCESS] lin(g)(" + xp + ") == " + "lin(g)(" + xpp + ")")

        } else {
          println("[FAIL] lin(g)(" + xp + ") != " + "lin(g)(" + xpp + ")")
        }
        println("[INFO] lin(g)(" + xp + ") = " + l1)
        println("[INFO] lin(g)(" + xpp + ") = " + l2)
        res
      }
      case _ => true
    })
  }

  class UndefinedLinearityException(xt: List[String], xtt: List[String], pt: List[String], xi: String) extends Exception {
    override def toString: String =
      "UndefinedLinearityException: Undefined Lin(G," + xt + "," + pt + ")(" + xi + ")"
  }

  def lin(g: GlobalProtocol)(x: String) = {
    new ReceiverOutput(l(g, List(), List(), List(), x))
  }

  private def l(g: GlobalProtocol, xm: List[String], xj: List[String], pt: List[String], xi: String): Set[(String, String, List[String])] = {
    val (lHash, rHash) = g.getHashes
//    println("Lin(G," + xm + ", " + pt + ")(" + xi + ")")
    val e = lHash(xi)
//    println(e)
    e match {
      case Message(x, p, pp, m, u, xp) if (!pt.contains(pp)) => Set((p + pp, m, xj)) ++ l(g, xm, xj, pt :+ pp, xp)
      case Message(x, p, pp, m, u, xp) => l(g, xm, xj, pt, xp)
      case Choice(x, xp, xpp) => l(g, xm, xj, pt, xp) ++ l(g, xm, xj, pt, xpp)
      case Parallel(x, xp, xpp) => l(g, xm, xj, pt, xp) ++ l(g, xm, xj, pt, xpp)
      case ChoiceJoin(x, xp, xpp) if (!xm.contains(xpp)) => l(g, xm :+ xpp, xj, pt, xpp)
      case ChoiceJoin(x, xp, xpp) if (xm.contains(xpp)) => Set()
      case ParallelJoin(x, xp, xpp) => l(g, xm, xj :+ xpp, pt, xpp)
      case End(x) => Set()
      case _ => throw new UndefinedLinearityException(xm, xj, pt, xi)
    }

  }
}