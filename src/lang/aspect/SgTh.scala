package lang.aspect

import lang.GlobalProtocol
import lang.Message

/**
 * This condition is true, if for all M such that match(pc,M), adv is
 * single-threaded wrt M
 */
object SgTh {

  def apply(g: GlobalProtocol, a: Aspect): Boolean = {
    false
  }

  private[this] def singleThread(m: Message): Boolean = {
    false
  }

}