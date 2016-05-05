package edu.thu.ss.logic.analysis

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.util.LogicUtils._

case class FormulaSimplifier(maxIterations: Int = 100) extends SequentialAnalyzer {
  protected val rules: Seq[SimplifyRule] = Seq(
    SimplifyAnd,
    SimplifyOr,
    SimplifyImply,
    SimplifyNot,
    SimplifyQuantifier,
    SimplifyFutureTemporal,
    SimplifyPastTemporal,
    SimplifyLTLFutureTemporal,
    SimplifyLTLPastTemporal)

  //mine
  def simplifyTorF(formula: Formula): Formula ={
    val startTime = System.currentTimeMillis()

    var curFormula = formula
    val startFormula = curFormula
    var iteration = 1
    var lastFormula = curFormula
    var continue = true
    
    println("----formula-------"+formula)
    while (continue) {
      curFormula = rules.foldLeft(curFormula) {
        case (formula, rule) =>
          val result = rule(formula)

          if (!result.fastEquals(formula)) {
            logTrace(
              s"""
                  |=== Applying Rule ${rule.ruleName} ===
                  |${sideBySide(formula.treeString, result.treeString).mkString("\n")}
                """.stripMargin)
          }
          result
      }
      iteration += 1
      if (iteration > maxIterations) {
        logInfo(s"Max iterations (${iteration - 1}) reached.")
        continue = false
      }

      if (curFormula.fastEquals(lastFormula)) {
        logTrace(
          s"Fixed point reached after ${iteration - 1} iterations.")
        continue = false
      }
      lastFormula = curFormula
    }
    val totalTime = System.currentTimeMillis - startTime
    logTrace(s"Formula simplification finishes in ${totalTime} ms.")

    curFormula
  }
    
  protected def analyzeFormula() {
    val startTime = System.currentTimeMillis()

    var curFormula = this.curFormula.formula
    val startFormula = curFormula
    var iteration = 1
    var lastFormula = curFormula
    var continue = true

    while (continue) {
      curFormula = rules.foldLeft(curFormula) {
        case (formula, rule) =>
          val result = rule(formula)

          if (!result.fastEquals(formula)) {
            logTrace(
              s"""
                  |=== Applying Rule ${rule.ruleName} ===
                  |${sideBySide(formula.treeString, result.treeString).mkString("\n")}
                """.stripMargin)
          }
          result
      }
      iteration += 1
      if (iteration > maxIterations) {
        logInfo(s"Max iterations (${iteration - 1}) reached.")
        continue = false
      }

      if (curFormula.fastEquals(lastFormula)) {
        logTrace(
          s"Fixed point reached after ${iteration - 1} iterations.")
        continue = false
      }
      lastFormula = curFormula
    }
    val totalTime = System.currentTimeMillis - startTime
    logTrace(s"Formula simplification finishes in ${totalTime} ms.")

    this.curFormula.formula = curFormula
  }

}

sealed abstract class SimplifyRule {

  val ruleName: String = {
    val className = getClass.getName
    if (className endsWith "$") className.dropRight(1) else className
  }

  def apply(formula: Formula): Formula
}

object SimplifyAnd extends SimplifyRule {

  def apply(formula: Formula): Formula = formula.transform {
    case And(True, right) => right
    case And(left, True) => left
    case And(False, right) => False
    case And(left, False) => False
    case And(left, right) if left == right => left
    case And(left, right) if left == Not(right) => False
  }
}

object SimplifyOr extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {
    case Or(True, right) => True
    case Or(left, True) => True
    case Or(False, right) => right
    case Or(left, False) => left
    case Or(left, right) if left == right => left
    case Or(left, right) if left == Not(right) => True
  }
}

object SimplifyImply extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {
    case Imply(True, right) => right
    case Imply(False, right) => True
    case Imply(left, True) => True
    //case Imply(left, False)=> Not(left)

    case Imply(left, right) if (left == right) => True

  }
}

object SimplifyNot extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {
    case Not(True) => False
    case Not(False) => True
    case Not(Not(child)) => child
  }

}

object SimplifyQuantifier extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {
    case quantifier: Quantifier if (unused(quantifier.variable, quantifier.child)) =>
      quantifier.child
  }

  private def unused(variable: Variable, formula: Formula): Boolean = formula.forall {
    case func: BaseFunctionCall => func.parameters.forall { unused(variable, _) }
    case v: Variable if (variable == v) => false
    case _ => true
  }

}

object SimplifyFutureTemporal extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {

    case AF(AF(child)) => AF(child)
    case AG(AG(child)) => AG(child)

    case AF(AG(AF(child))) => AG(AF(child))
    case AG(AF(AG(child))) => AF(AG(child))
    case Or(AF(left), AF(right)) => AF(Or(left, right))
    case And(AG(left), AG(right)) => AG(And(left, right))

    case And(AX(left), AX(right)) => AX(And(left, right))
    case Or(AX(left), AX(right)) => AX(Or(left, right))
    case And(EX(left), EX(right)) => EX(And(left, right))
    case Or(EX(left), EX(right)) => EX(Or(left, right))

    case AF(True) => True
    case AF(False) => False
    case AG(True) => True
    case AG(False) => False
    case AX(True) => True
    case AX(False) => False
    case EX(True) => True
    case EX(False) => False

    case AU(True, right) => AF(right)
    case AU(False, right) => False
    case AU(left, True) => True
    case AU(left, False) => False
  }

}

object SimplifyPastTemporal extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {

    case pAF(pAF(child)) => pAF(child)
    case pEF(pEF(child)) => pEF(child)
    case pAG(pAG(child)) => pAG(child)
    case pEG(pEG(child)) => pEG(child)

    //  case AF(AG(AF(child))) => AG(AF(child))
    //  case AG(AF(AG(child))) => AF(AG(child))
    case Or(pAF(left), pAF(right)) => pAF(Or(left, right))
    case Or(pEF(left), pEF(right)) => pEF(Or(left, right))

    case And(pAG(left), pAG(right)) => pAG(And(left, right))

    case And(pAX(left), pAX(right)) => pAX(And(left, right))
    //  case And(pEX(left), EX(right)) => pEX(And(left, right))
    case Or(pAX(left), pAX(right)) => pAX(Or(left, right))
    case Or(pEX(left), pEX(right)) => pEX(Or(left, right))

    case pAF(True) => True
    case pAF(False) => False
    case pAG(True) => True
    case pAG(False) => False
    case pAX(True) => True
    case pAX(False) => False
    case pEX(True) => True
    case pEX(False) => False

    case pAS(True, right) => pAF(right)
    case pAS(False, right) => False
    case pES(True, right) => pEF(right)
    case pES(False, right) => False

    case pAS(left, True) => True
    case pAS(left, False) => False
    case pES(left, True) => True
    case pES(left, False) => False
  }
}


object SimplifyLTLFutureTemporal extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {

    case F(interval1, F(interval2, child)) => F(addInterval(interval1, interval2),child)
    case F(interval, child) => U(interval, True, child)
    
    case G(interval1, G(interval2, child)) => G(addInterval(interval1, interval2),child)
    case G(interval, child) => Not(F(interval,Not(child)))

    case F(interval, True) => True
    case F(interval, False) => False
    case G(interval, True) => True
    case G(interval, False) => False
    case X(interval, True) => True
    case X(interval, False) => False
    
    case U(interval, False, right) => False
    case U(interval, left, True) => True
    case U(interval, left, False) => False
    
    

  }
  
  def addInterval(interval1: Seq[String], interval2: Seq[String]): Seq[String] ={
    Seq( ( interval1.head.toInt + interval2.head.toInt ).toString(), 
        ( interval1.last.toInt + interval2.last.toInt ).toString())
  }

}

object SimplifyLTLPastTemporal extends SimplifyRule {
  def apply(formula: Formula): Formula = formula.transform {

    case pF(interval1, pF(interval2, child)) => pF(addInterval(interval1, interval2), child)
    case pF(interval, child) => pU(interval, True, child)
    
    case pG(interval1, pG(interval2, child)) => pG(addInterval(interval1, interval2), child)
    case pG(interval, child) => Not(pF(interval,Not(child)))


    case pF(interval, True) => True
    case pF(interval, False) => False
    case pG(interval, True) => True
    case pG(interval, False) => False
    case pX(interval, True) => True
    case pX(interval, False) => False


    case pU(interval, False, right) => False
    case pU(interval, left, True) => True
    case pU(interval, left, False) => False
    
    

  }
  
  def addInterval(interval1: Seq[String], interval2: Seq[String]): Seq[String] ={
    Seq( ( interval1.head.toInt + interval2.head.toInt ).toString(), 
        ( interval1.last.toInt + interval2.last.toInt ).toString())
  }
  
}

