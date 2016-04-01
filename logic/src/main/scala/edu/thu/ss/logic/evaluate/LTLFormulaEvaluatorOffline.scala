package edu.thu.ss.logic.evaluate

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.model.State
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.util.LogicUtils
import scala.collection.mutable.ListBuffer
/**
 * a context for each model  
 */
class LTLEvaluationContext(val trace: Seq[QueryModel]) {

  private val valuation = new ListBuffer[mutable.HashMap[Formula, Boolean]]


  def clear {
    valuation.clear
  }
  def printf() {
    println(valuation.length)
    var i = 0;
    valuation.foreach{
      s => {
        i += 1
        println(s"$i－－－－－：$s")
      }
    }
  }
  
  def getValue(index:Int, formula: Formula) = valuation(index).get(formula)

  def setValue(index:Int, formula: Formula, value: Boolean) {
    if (index < valuation.length){
      valuation(index).put(formula, value)
    }
    else {
      valuation += new mutable.HashMap[Formula, Boolean]
      valuation(index).put(formula, value)
      println(valuation.length)
      println(s"$index,  $formula,  $value")
    }
  }
  
  def getLength() = valuation.length


}

// offline

class LTLFormulaEvaluatorOffline(val trace: Seq[QueryModel]) extends Logging {

  private lazy val context = new LTLEvaluationContext(trace)

  def evaluate(formula: Formula): Boolean = {
    val value = trace.forall { s=> evaluateFormula(formula, s, trace.indexOf(s))}
    context.printf()
    //the context and cache are shared among all evaluations of a formula on a model (multiple initial states)
    context.clear
//    model.clearCache

    value
  }

  private def evaluateFormula(formula: Formula, model: QueryModel, index :Int): Boolean = {

    if (index < context.getLength()) {
      val cacheResult = context.getValue(index,formula)
      if (cacheResult.isDefined) {
        logTrace(s"cache hit for query$index formula $formula")
        return cacheResult.get
      }
    }
    

    val result =
      formula match {
        case Not(child) =>
          !(evaluateFormula(child, model, index))

        case And(left, right) =>
          evaluateFormula(left, model, index) && evaluateFormula(right, model, index)

        case Or(left, right) =>
          evaluateFormula(left, model, index) || evaluateFormula(right, model, index)

        case Imply(left, right) =>
          !evaluateFormula(left, model, index) || evaluateFormula(right, model, index)


//        case variable: Variable =>
//          //must be a boolean variable
//          context.getValue(variable).asInstanceOf[Boolean]

        case True => true

        case False => false

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          val upper = x.upper
          val lower = x.lower
          index+1 == trace.length || 
            (trace(index+1).timestamp - trace(index).timestamp <= upper
                && trace(index+1).timestamp - trace(index).timestamp >= lower
                && evaluateFormula(x.child, trace(index+1), index+1))

        case _: U =>
          val u = formula.asInstanceOf[U]
          val upper = u.upper
          val lower = u.lower
          if (evaluateFormula(u.right, model, index)) {
            u.newListSatisfied = trace(index).timestamp
            println(u.newListSatisfied)
            true
          }
          else {
            index+1 < trace.length && evaluateFormula(u.left, model, index) &&
            evaluateFormula(u, trace(index+1), index+1) &&
            u.newListSatisfied - trace(index).timestamp <= upper && 
            u.newListSatisfied - trace(index).timestamp >= lower
          }
        
        case _: pX =>
          val px = formula.asInstanceOf[pX]
          val upper = px.upper
          val lower = px.lower
          index-1 <0 || 
            (trace(index).timestamp - trace(index-1).timestamp <= upper
                && trace(index).timestamp - trace(index-1).timestamp >= lower
                && evaluateFormula(px.child, trace(index-1), index-1))
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          val upper = pu.upper
          val lower = pu.lower
          if (evaluateFormula(pu.right, model, index)) {
            pu.newListSatisfied = trace(index).timestamp
            println(pu.newListSatisfied)
            true
          }
          else {
            index-1 >= 0 && evaluateFormula(pu.left, model, index) &&
            evaluateFormula(pu, trace(index-1), index-1) &&
            trace(index).timestamp - pu.newListSatisfied<= upper && 
            trace(index).timestamp - pu.newListSatisfied>= lower
          }
          
        case _ =>
          val modelEvaluator = new FormulaEvaluator(model, true)
          modelEvaluator.evaluate(formula)

      }
    
    logTrace(s"query$index cache formula $formula")
    context.setValue(index, formula, result)
    result
  }


}