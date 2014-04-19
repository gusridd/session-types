package lang

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.Set
import scala.annotation.tailrec

class GlobalProtocol(val exprs: List[expr]) {

  private val x0: String = "x_0"
  private val end: String = "end"
  private val xs: HashSet[String] = HashSet() ++ Collector.collectStateVariables(GlobalProtocol.this)

  //  sanityCheck()

  override def toString(): String = exprs.toString

  def contains(x: String): Boolean = xs contains x

  def sanityCheck() = {
    val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
    var endCount = 0
    /**
     * Appearances of each variable is counted so the sanity conditions become
     * easy to check.
     */
    exprs foreach {
      case Message(x1, _, _, _, _, x2) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
      case Choice(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case ChoiceJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case Parallel(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1, m(x2)._2 + 1)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case ParallelJoin(x1, x2, x3) =>
        m(x1) = (m(x1)._1 + 1, m(x1)._2)
        m(x2) = (m(x2)._1 + 1, m(x2)._2)
        m(x3) = (m(x3)._1, m(x3)._2 + 1)
      case End(x) =>
        m(x) = (m(x)._1 + 1, m(x)._2)
        endCount = endCount + 1
    }
    /**
     * There must exist at most one end variable.
     */
    if (endCount > 1) throw new SanityConditionException("UniqueEnd: end appears more than once")

    /**
     * Each variable must appear exactly once at the left and the right side
     * of an equation. Except for the end variable.
     */
    val unambiguous = m filter {
      case (k, (v1, v2)) if k != x0 => v1 != 1 || v2 != 1
      case _ => false
    }

    if (!unambiguous.isEmpty) {
      throw new SanityConditionException("Unanbiguity: ambiguous definition at " + unambiguous.head._1)
    }

    if (m(x0)._1 != 1 && m(x0)._2 != 0) {
      throw new SanityConditionException("Unique start: x_0 must appear exactly once, on the left-hand side")
    }
  }

  private var hashCacheL: Option[HashMap[String, lang.expr]] = None
  private var hashCacheR: Option[HashMap[String, lang.expr]] = None

  def getHashes(): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
    (hashCacheL, hashCacheR) match {
      case (Some(hl), Some(hr)) => return (hl, hr)
      case _ => {
        val (leftHash, rightHash) = getHashesFromExpr(exprs)
        hashCacheL = Some(leftHash)
        hashCacheR = Some(rightHash)
        (leftHash, rightHash)
      }
    }
  }

  def getHashesFromExpr(exprs: List[expr] = exprs): (HashMap[String, lang.expr], HashMap[String, lang.expr]) = {
    val leftHash = HashMap[String, expr]()
    val rightHash = HashMap[String, expr]()
    exprs foreach {
      case m @ Message(x1, _, _, _, _, x2) => {
        leftHash(x1) = m
        rightHash(x2) = m
      }
      case e @ End(x) => {
        leftHash(x) = e
      }
      case c @ Continue(x1, x2) => {
        leftHash(x1) = c
        rightHash(x2) = c
      }
      case c @ Choice(x1, x2, x3) => {
        leftHash(x1) = c
        rightHash(x2) = c
        rightHash(x3) = c
      }
      case p @ Parallel(x1, x2, x3) => {
        leftHash(x1) = p
        rightHash(x2) = p
        rightHash(x3) = p
      }
      case cj @ ChoiceJoin(x1, x2, x3) => {
        leftHash(x1) = cj
        leftHash(x2) = cj
        rightHash(x3) = cj
      }
      case pj @ ParallelJoin(x1, x2, x3) => {
        leftHash(x1) = pj
        leftHash(x2) = pj
        rightHash(x3) = pj
      }
    }
    (leftHash, rightHash)
  }

  def getParticipants(): Set[String] = {
    val s = Set[String]()
    exprs foreach {
      case m @ Message(_, a, b, _, _, _) => {
        (s += a) += b
      }
      case _ =>
    }
    s
  }

  def threadReduction() = {

    def getHash(exprs: List[expr]) = {
      val leftHash = HashMap[String, expr]()
      exprs foreach {
        case e => {
          leftHash += (e.left -> e)
        }
      }
      leftHash
    }

    val hash = getHash(exprs)

    /**
     * reduce:
     * exprs: list of expressions to reduce
     * return the list of the reduced expressions and a boolean representing if a reduction has been applied
     */
    def reduce(exprs: List[expr]): (List[expr], Boolean) = {
      exprs foreach {
        case m @ Message(x1, _, _, _, _, x2) =>
          return { /*println("[Trans]"); */ ((exprs filterNot (_ == m)).map(_.substitute(x1, x2)), true) }
        case p @ Parallel(x1, x2, x3) => hash.get(p.right) match {
          case Some(pj @ ParallelJoin(x2p, x3p, x4p)) =>
            return { println("[Bra]"); ((exprs filter (x => x != p && x != pj)).map(_.substitute(x1, x4p)), true) }
          case None =>
        }
        case c @ Choice(x1, x2, x3) => hash.get(c.right) match {
          case Some(cj @ ChoiceJoin(x2p, x3p, x4p)) =>
            return {
              //              println("[Bra]: " + c + " AND " + cj)
              //              println("nonfiltered " + exprs)
              //              val filtered = (exprs filter (x => c != x && x != cj))
              //              println("filtered " + filtered)
              ((exprs filter (x => x != c && x != cj)).map(_.substitute(x1, x4p)), true)
            }
          case Some(_) =>
          case None =>
        }
        case rec @ ChoiceJoin(x1, x2, x3) => { println("maybeRec:" + rec + "\nhash:" + hash.get(rec.right)); hash.get(rec.right) } match {
          case Some(recj @ Choice(x3p, x4p, x2p)) if (x3 == x3p && (x2 == x4p || x2 == x2p)) =>
            return { println("[Rec]"); ((exprs filter (x => x != rec && x != recj)).map(_.substitute(x1, x4p)), true) }
          case Some(recj @ Choice(x3p, x4p, x2p)) if (x3 == x3p && (x1 == x4p || x1 == x2p)) =>
            return { println("[Rec]"); ((exprs filter (x => x != rec && x != recj)).map(_.substitute(x2, x4p)), true) }
          case Some(_) =>
          case None =>
        }
        case e @ End(x) => {
          // end should not be substituted until the end
          println("[End] " + e);
          //          return ((exprs filterNot (x => x == e)).map(_.substitute(e.left, e.right)), true)
          //return (exprs,false)
        }
        case _ =>
      }
      (exprs, false)
    }

    def STReduction(exprs: List[expr]): (List[expr], Boolean) = {
      val (leftHash, rightHash) = getHashesFromExpr(exprs)

      /**
       * Parallel graph cover
       */
      def TReduction(e: expr): (Set[expr], Boolean, String, String) = {
        println("TReduction")
        val possibleSet: Set[expr] = ParallelCover(e, Set())
        println("ParallelCover for: " + e)
        possibleSet foreach { x => println(x.canonical) }
        val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
        /**
         * Appearances of each variable is counted so the sanity conditions become
         * easy to check.
         */
        possibleSet foreach {
          case e@Message(x1, _, _, _, _, x2) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
          case e@Choice(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@ChoiceJoin(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1 + 1, m(x2)._2)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@Parallel(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@ParallelJoin(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1 + 1, m(x2)._2)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@End(x) =>
            m(x) = (m(x)._1 + 1, m(x)._2)
        }

        val inputNodes = m filter {
          case (k, (v1, v2)) => v1 == 1 && v2 == 0
          case _ => false
        }

        val outputNodes = m filter {
          case (k, (v1, v2)) => v1 == 0 && v2 == 1
          case _ => false
        }
        
        println("inputNodes")
        inputNodes foreach { x => println(x._1) }
        println("outputNodes")
        outputNodes foreach { x => println(x._1) }

        if (inputNodes.size == 1 && outputNodes.size == 1) {
          (possibleSet, true, inputNodes.head._1, outputNodes.head._1)
        } else {
          (Set(), false, "", "")
        }
      }

      def variableSet(exprs: Set[expr]) = {
        val s: Set[String] = Set()
        exprs foreach (e => s ++ e.getVariables)
        s
      }

      /**
       * Choice graph cover
       */
      def SReduction(e: expr): (Set[expr], Boolean, String, String) = {
        println("SReduction")
        val possibleSet: Set[expr] = ChoiceCover(e, Set())
        println("ChoiceCover for: " + e)
        possibleSet foreach { x => println(x.canonical) }
        val m: scala.collection.mutable.Map[String, (Int, Int)] = collection.mutable.Map() ++ ((xs map (t => (t, (0, 0)))) toMap);
        /**
         * Appearances of each variable is counted so the sanity conditions become
         * easy to check.
         */
        possibleSet foreach {
          case e@Message(x1, _, _, _, _, x2) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
          case e@Choice(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@ChoiceJoin(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1 + 1, m(x2)._2)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@Parallel(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1, m(x2)._2 + 1)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@ParallelJoin(x1, x2, x3) =>
            println("possibleSet foreach:" + e)
            m(x1) = (m(x1)._1 + 1, m(x1)._2)
            m(x2) = (m(x2)._1 + 1, m(x2)._2)
            m(x3) = (m(x3)._1, m(x3)._2 + 1)
          case e@End(x) =>
            println("possibleSet foreach:" + e)
            m(x) = (m(x)._1 + 1, m(x)._2)
        }

        val inputNodes = m filter {
          case (k, (v1, v2)) => v1 == 1 && v2 == 0
          case _ => false
        }

        val outputNodes = m filter {
          case (k, (v1, v2)) => v1 == 0 && v2 == 1
          case _ => false
        }
        println("inputNodes")
        inputNodes foreach { x => println(x._1) }
        println("outputNodes")
        outputNodes foreach { x => println(x._1) }

        if (inputNodes.size == 1 && outputNodes.size == 1) {
          (possibleSet, true, inputNodes.head._1, outputNodes.head._1)
        } else {
          (Set(), false, "","")
        }
      }

      def ChoiceCover(e: expr, s: Set[expr]): Set[expr] = {
        if (s.contains(e)) {
          return s
        }
        e match {
          case Choice(x1, x2, x3) => ChoiceCover(leftHash(x2), s + e) ++ ChoiceCover(leftHash(x3), s + e)
          case ChoiceJoin(x1, x2, x3) => ChoiceCover(leftHash(x3), s + e)
          case Message(x1, _, _, _, _, x2) => ChoiceCover(leftHash(x2), s + e)
          case Parallel(x1, x2, x3) => s
          case ParallelJoin(x1, x2, x3) => s
          case End(x) => s
          case Continue(x1, x2) => Set(e) ++ ChoiceCover(leftHash(x2), s + e)
        }
      }

      def ParallelCover(e: expr, s: Set[expr]): Set[expr] = {
        if (s.contains(e)) {
          return s
        }
        e match {
          case Choice(x1, x2, x3) => s
          case ChoiceJoin(x1, x2, x3) => s
          case Message(x1, _, _, _, _, x2) => ParallelCover(leftHash(x2), s + e)
          case Parallel(x1, x2, x3) => ParallelCover(leftHash(x2), s + e) ++ ParallelCover(leftHash(x3), s + e)
          case ParallelJoin(x1, x2, x3) => ParallelCover(leftHash(x3), s + e)
          case End(x) => s
          case Continue(x1, x2) => Set(e) ++ ParallelCover(leftHash(x2), s + e)
        }
      }

      exprs foreach {
        case e @ Parallel(x1, x2, x3) => {
          val (set, result, first, last) = TReduction(e)
          if (result) {
            println("SET, FIRST AND LAST")
            println(set)
            println(first)
            println(last)
            println("filtered")
            println(exprs filterNot (e => set.contains(e)))
            return ((exprs filterNot (e => set.contains(e))).map(_.substitute(x1, last)), true)
          }
        }
        case e @ Choice(x1, x2, x3) => {
          val (set, result, first, last) = SReduction(e)
          if (result) {
            println("SET, FIRST AND LAST")
            println(set)
            println(first)
            println(last)
            println("filtered")
            println(exprs filterNot (e => set.contains(e)))
            return ((exprs filterNot (e => set.contains(e))).map(_.substitute(x1, last)), true)
          }
        }
        case e @ ChoiceJoin(x1, x2, x3) => {
          val (set, result, first, last) = SReduction(e)
          if (result) {
            println("SET, FIRST AND LAST")
            println(set)
            println(first)
            println(last)
            println("filtered")
            println(exprs filterNot (e => set.contains(e)))
            return ((exprs filterNot (e => set.contains(e))).map(_.substitute(x1, last)), true)
          }
        }
        case _ => // Do nothing
      }

      (exprs, false)
    }

    try {
      /**
       * The first element in the pair represents the current state of the
       * reduction, the second one if the reduction steps should continue.
       */
      var reduction = (exprs, true)

      println("****************\nPROTOCOL\n****************\n")
      exprs foreach { x => println(x.canonical) }

      while (reduction._2) {

        println("****************\nSIMPLE REDUCTION\n****************\n")
        //println("reduction: " + exprs)
        println()
        exprs foreach { x => println(x.canonical) }
        reduction = reduce(reduction._1)

        while (reduction._2) {
          //println("reduction: " + reduction._1)
          println()
          reduction._1 foreach { x => println(x.canonical) }
          reduction = reduce(reduction._1)
        }

        println("************\nST REDUCTION\n************\n")
        reduction._1 foreach { x => println(x.canonical) }
        reduction = STReduction(reduction._1)
        while (reduction._2) {
          //println("reduction: " + reduction._1)
          println()
          reduction._1 foreach { x => println(x.canonical) }
          reduction = STReduction(reduction._1)
        }

        reduction = reduce(reduction._1)
      }

      val reductedList = reduction._1

      if (reductedList.length > 0 && !(reductedList.length == 1 && reductedList(0).isEnd))
        throw new SanityConditionException("Thread correctness: unable to reduce more " + reduction._1)
      println("*******\nSUCCESS\n*******\n")
    } catch {
      case e: java.util.NoSuchElementException => throw e
    }
  }

}