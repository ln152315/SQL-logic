package edu.thu.ss.logic.evaluate

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.analysis._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.model.State
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.util.LogicUtils
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
/**
 * a context for each model  
 */
//class LTLEvaluationContext(val trace: Seq[QueryModel]) {
//
//  private val valuation = new ListBuffer[mutable.HashMap[Formula, Boolean]]
//
//
//  def clear {
//    valuation.clear
//  }
//  def printf() {
//    println(valuation.length)
//    var i = 0;
//    valuation.foreach{
//      s => {
//        i += 1
//        println(s"$i－－－－－：$s")
//      }
//    }
//  }
//  
//  def getValue(index:Int, formula: Formula) = valuation(index).get(formula)
//
//  def setValue(index:Int, formula: Formula, value: Boolean) {
//    if (index < valuation.length){
//      valuation(index).put(formula, value)
//    }
//    else {
//      valuation += new mutable.HashMap[Formula, Boolean]
//      valuation(index).put(formula, value)
//      println(valuation.length)
//      println(s"$index,  $formula,  $value")
//    }
//  }
//  
//  def getLength() = valuation.length
//
//
//}

// online

class LTLFormulaEvaluatorOnline(val trace: Seq[QueryModel]) extends Logging {
  private val parameterMap = new HashMap[Formula, Formula]

  private def findParameters(formula: Formula){
      formula match {
        case Not(child) =>
          if (!parameterMap.contains(formula)) parameterMap(formula) = True
          findParameters(child)

        case And(left, right) =>
          if (!parameterMap.contains(formula)) parameterMap(formula) = True
          findParameters(left)
          findParameters(right)

        case Or(left, right) =>
          if (!parameterMap.contains(formula)) parameterMap(formula) = True
          findParameters(left)
          findParameters(right)

        case Imply(left, right) =>
          if (!parameterMap.contains(formula)) parameterMap(formula) = True
          findParameters(left)
          findParameters(right)

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          val upper = x.upper.toInt
          val lower = x.lower.toInt
          for(i<- 0 to lower){
            for(j<- 0 to upper){
              if(i <= j){
                val tmpX = X(Seq(i.toString(), j.toString()), x.child)
//                tmpX.upper = j
//                tmpX.lower = i
                if (!parameterMap.contains(tmpX)) parameterMap(tmpX) = True
                findParameters(x.child)
              }
              
            }
          }

        case _: U =>
          println("----tag")
          val u = formula.asInstanceOf[U]
          val upper = u.upper.toInt
          val lower = u.lower.toInt
          for(i<- 0 to lower){
            for(j<- 0 to upper){
              if(i <= j){
                val tmpU = U(Seq(i.toString(), j.toString()), u.left, u.right)
//                tmpU.upper = j
//                tmpU.lower = i
                println("---i:"+tmpU.lower+"---j:"+tmpU.upper)
                println("---tmpU----"+tmpU)
                if (!parameterMap.contains(tmpU)) {
                  println("success")
                  parameterMap(tmpU) = True
                  findParameters(u.left)
                  findParameters(u.right)
                }
              }
              
            }
          }
        
        case _: pX =>
          val px = formula.asInstanceOf[pX]
          val upper = px.upper.toInt
          val lower = px.lower.toInt
          for(i<- 0 to lower){
            for(j<- 0 to upper){
              if(i <= j){
                val tmpPX = pX(Seq(i.toString(), j.toString()), px.child)
//                tmpPX.upper = j
//                tmpPX.lower = i
                if (!parameterMap.contains(tmpPX)) parameterMap(tmpPX) = True
                findParameters(px.child)
              }
              
            }
          }
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          val upper = pu.upper.toInt
          val lower = pu.lower.toInt
          for(i<- 0 to lower){
            for(j<- 0 to upper){
              if(i <= j){
                val tmpPU = pU(Seq(i.toString(), j.toString()), pu.left, pu.right)
//                tmpPU.upper = j
//                tmpPU.lower = i
                if (!parameterMap.contains(tmpPU)) parameterMap(tmpPU) = True
                findParameters(pu.left)
                findParameters(pu.right)
              }
              
            }
          }
          
        case _ =>
          if (!parameterMap.contains(formula)) parameterMap(formula) = True

      }
  }
  
  def monitor(formulaVal: Formula): Formula = {
    parameterMap.clear()
    var formula = formulaVal
    findParameters(formula)
    
    val resolveMap = parameterMap.clone()
    val deriveMap = parameterMap.clone()
    println("---initial:parameterMap-----"+parameterMap)
    println("---initial:resolveMap-----"+resolveMap)
    println("---initial:deriveMap-----"+deriveMap)
    var break = false 
    var i=0
    while (i < trace.length && !break){
      println("---parameterMap-----"+parameterMap)
      for ((k, v)<-parameterMap){
        resolveMap(k) = resolve(k, trace, resolveMap, deriveMap, i)
      }
      println("----resolveMap-----"+resolveMap)
      for ((k, v)<-parameterMap){
        deriveMap(k) = derive(k, trace, resolveMap, deriveMap, i)
      }
      println("----deriveMap-----"+deriveMap)
      println("----formula-----"+formula)
      formula =  if (deriveMap.contains(formula)) deriveMap(formula) else null
      
      val simplifyAnalyzers = FormulaSimplifier()
      formula = simplifyAnalyzers.simplifyTorF(formula)
      
      //判断之前可以调之前，化简的函数，写一个更加简单的，只包含T,F关系的
      if (formula == True || formula == False) {
        break = true
      }
      i = i+1
        
    }
    formula
  }
  
  private def resolve(formula: Formula,trace: Seq[QueryModel], resolveMap: HashMap[Formula, Formula], deriveMap: HashMap[Formula, Formula], index: Int): Formula = {
    
    val result =
      formula match {
        case Not(child) =>
          Not(resolve(child, trace, resolveMap, deriveMap, index))

        case And(left, right) =>
          And(resolve(left, trace, resolveMap, deriveMap, index), resolve(right, trace, resolveMap, deriveMap, index))

        case Or(left, right) =>
          Or(resolve(left, trace, resolveMap, deriveMap, index), resolve(right, trace, resolveMap, deriveMap, index))

        case Imply(left, right) =>
          Or(Not(resolve(left, trace, resolveMap, deriveMap, index)), resolve(right, trace, resolveMap, deriveMap, index))

        case True => True

        case False => False

        //temporal part
        case _: X =>
          formula

        case _: U =>
          formula
        
        case _: pX =>
          val px = formula.asInstanceOf[pX]
          val upper = px.upper
          val lower = px.lower
          if (index == 0 
              || !(trace(index-1).timestamp <= upper && trace(index-1).timestamp >= lower)){
            False
          }
          else {
            resolveMap(deriveMap(px.child))
          }
        
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          val upper = pu.upper
          val lower = pu.lower
          var formula1 = formula
          var formula2 = formula
          if(0 <= upper && 0 >= lower) {
            formula2 = resolveMap(pu.right)
          }
          else{
            formula2 = False
          }
          if(index == 0){
            formula1 = False
          }
          else{
            // 有问题？？？
            val timeSubtract = trace(index).timestamp - trace(index-1).timestamp
            if (0 <= (upper - timeSubtract)){
              pu.upper = upper - timeSubtract
              pu.lower = lower - timeSubtract
              formula1 = And(resolveMap(pu.left), resolveMap(deriveMap(pu))) 
            }
            else{
              formula1 = False
            }
          }
          Or(formula1, formula2)
          
        case _ =>
          val modelEvaluator = new FormulaEvaluator(trace(index), true)
          val tmpresult = modelEvaluator.evaluate(formula)
          if (tmpresult) True
          else False
    }
      
    result
  }
  
  private def derive(formula: Formula,trace: Seq[QueryModel], resolveMap: HashMap[Formula, Formula], deriveMap: HashMap[Formula, Formula], index: Int): Formula = {
        
    val result =
      formula match {
        case Not(child) =>
          Not(derive(child, trace, resolveMap, deriveMap, index))

        case And(left, right) =>
          And(derive(left, trace, resolveMap, deriveMap, index), derive(right, trace, resolveMap, deriveMap, index))

        case Or(left, right) =>
          Or(derive(left, trace, resolveMap, deriveMap, index), derive(right, trace, resolveMap, deriveMap, index))

        case Imply(left, right) =>
          Or(Not(derive(left, trace, resolveMap, deriveMap, index)), derive(right, trace, resolveMap, deriveMap, index))

        case True => True

        case False => False

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          val upper = x.upper
          val lower = x.lower
          if (index == trace.size-1 
              || !(trace(index+1).timestamp <= upper && trace(index+1).timestamp >= lower)){
            False
          }
          else {
            x.child
          }

        case _: U =>
          val u = formula.asInstanceOf[U]
          val upper = u.upper
          val lower = u.lower
          var formula1 = formula
          var formula2 = formula
          if(0 <= upper && 0 >= lower) {
            formula1 = resolveMap(u.right)
          }
          else{
            formula1 = False
          }
          if(index == trace.size-1 ){
            formula2 = False
          }
          else{
            // 有问题？？？
            val timeSubtract = trace(index+1).timestamp - trace(index).timestamp
            if (0 <= (upper - timeSubtract)){
              u.upper = upper - timeSubtract
              u.lower = lower - timeSubtract
              formula2 = And(deriveMap(u.left), u) 
            }
            else{
              formula2 = False
            }
          }
          Or(formula1, formula2)
        
        case _: pX =>
          //有问题
          deriveMap(resolveMap(formula))
          
        case _: pU =>
          //有问题
          deriveMap(resolveMap(formula))
          
          
        case _ =>
          val modelEvaluator = new FormulaEvaluator(trace(index), true)
          val tmpresult = modelEvaluator.evaluate(formula)
          if (tmpresult) True
          else False
    }
      
    result
  } 

}