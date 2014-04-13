package lang

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.Set

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
    if (endCount > 1) throw new SanityConditionException("UniqueEnd: end appears more than once")
    //    exprs foreach { x => println(x.canonical) }
    //    println("m")
    //    println(m)
    val unambiguous = m filter {
      case (k, (v1, v2)) if k != x0 => v1 != 1 || v2 != 1
      case _ => false
    }
    //    println("unambiguous")
    //    println(unambiguous)

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
        val (leftHash,rightHash) = getHashesFromExpr(exprs)
        hashCacheL = Some(leftHash)
        hashCacheR = Some(rightHash)
        (leftHash,rightHash)
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

      val startingFlux = 1.0

      def TReduction(e: expr, flux: Double, assign: HashMap[expr, Double], toEliminate: Set[expr]): (Set[expr], Boolean, Option[ParallelJoin]) = {
        println("\nTReduction(" + flux + "): " + e)
        println("assign: " + assign)
        println("toEliminate: " + toEliminate)
        e match {
          case p @ Parallel(x1, x2, x3) => {
            val (leftSet, leftResult, leftSome) = TReduction(leftHash(x2), flux / 2.0, assign, toEliminate)
            val (rightSet, rightResult, rightSome) = TReduction(leftHash(x3), flux / 2.0, assign, toEliminate)
            // flux closing is done at the right side, thus rightSome is the only posible last 'expr'
            (leftSet ++ rightSet + p ++ toEliminate, leftResult || rightResult, rightSome)
          }
          case pj @ ParallelJoin(x1, x2, x3) => {
            if (!assign.contains(pj)) {
              assign(pj) = flux
              (toEliminate += pj, false, None)
            } else if (flux + assign(pj) != startingFlux) {
              // Go through and join flows
              TReduction(leftHash(x3), flux + assign(pj), assign, toEliminate += pj)
            } else {
              // A T-System has been found
              println("T-System found!!!")
              (toEliminate += pj, true, Some(pj))
            }
          }
          case Choice(x1, x2, x3) => (Set(), false, None)
          case ChoiceJoin(x1, x2, x3) => (Set(), false, None)
          case End(x) => (Set(), false, None)
          case Message(x1, _, _, _, _, x2) => throw new Exception("Messages should not exist at this point")
          case Continue(x1, x2) => throw new Exception("Continues should not exist at this point")
        }
      }

      def SReduction(e: expr, flux: Double, assign: HashMap[expr, Double], toEliminate: Set[expr]): (Set[expr], Boolean, Option[ChoiceJoin]) = {
        e match {
          case p @ Choice(x1, x2, x3) => {
            val (leftSet, leftResult, leftSome) = SReduction(leftHash(x2), flux / 2.0, assign, toEliminate)
            val (rightSet, rightResult, rightSome) = SReduction(leftHash(x3), flux / 2.0, assign, toEliminate)
            // flux closing is done at the right side, thus rightSome is the only posible last 'expr'
            (leftSet ++ rightSet + p ++ toEliminate, leftResult || rightResult, rightSome)
          }
          case pj @ ChoiceJoin(x1, x2, x3) => {
            if (!assign.contains(pj)) {
              assign(pj) = flux
              (toEliminate += pj, false, None)
            } else if (flux + assign(pj) != startingFlux) {
              // Go through and join flows
              SReduction(leftHash(x3), flux + assign(pj), assign, toEliminate += pj)
            } else {
              // A T-System has been found
              println("S-System found!!!")
              (toEliminate += pj, true, Some(pj))
            }
          }
          case Parallel(x1, x2, x3) => (Set(), false, None)
          case ParallelJoin(x1, x2, x3) => (Set(), false, None)
          case End(x) => (Set(), false, None)
          case Message(x1, _, _, _, _, x2) => throw new Exception("Messages should not exist at this point")
          case Continue(x1, x2) => throw new Exception("Continues should not exist at this point")
        }
      }

      exprs foreach {
        case first @ Parallel(x1, x2, x3) => {
          val (set, result, last) = TReduction(first, startingFlux, HashMap[expr, Double](), LinkedHashSet[expr]())
          if (result) {
            println("SET, FIRST AND LAST")
            println(set)
            println(first)
            println(last)
            println("filtered")
            println(exprs filterNot (e => set.contains(e)))
            return ((exprs filterNot (e => set.contains(e))).map(_.substitute(x1, last.get.x_3)), true)
          }
        }
        case first @ Choice(x1, x2, x3) => {
          val (set, result, last) = SReduction(first, startingFlux, HashMap[expr, Double](), LinkedHashSet[expr]())
          if (result) {
            println("SET, FIRST AND LAST")
            println(set)
            println(first)
            println(last)
            println("filtered")
            println(exprs filterNot (e => set.contains(e)))
            return ((exprs filterNot (e => set.contains(e))).map(_.substitute(x1, last.get.x_3)), true)
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