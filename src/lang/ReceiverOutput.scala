package lang

/**
 * The output class express that the equality
 * [A]Rcv(G)(x_1) = [B]Rcv(G)(x_2) holds if forall(p:l1:xt1) in A,
 * forall(p:l2:xt2) in B, l1 != l2 or x1p and x2p share a
 * non-null suffix
 */
case class ReceiverOutput(s: Set[(String, String, List[String])]) {
  override def equals(c: Any) = c match {
    case ReceiverOutput(sp) => s forall (e1 => sp forall (e2 => (e1, e2) match {
      case ((p1, l1, xt1), (p2, l2, xt2)) if (p1 == p2) => l1 != l2 || nonNullSuffix(xt1, xt2)
      case ((p1, l1, xt1), (p2, l2, xt2)) if (p1 != p2) => true
      case _ => false
    }))
    case _ => false
  }
  
  def isEmpty = s.isEmpty

  final private def nonNullSuffix(xt1: List[String], xt2: List[String]): Boolean =
    (xt1, xt2) match {
      case (List(), h2 :: t2) => false
      case (h1 :: t1, List()) => false
      case (List(), List()) => false
      case _ => xt1.last == xt2.last
    }
}