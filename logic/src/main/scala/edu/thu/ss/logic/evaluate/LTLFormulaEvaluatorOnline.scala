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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.thu.ss.logic.policy.Rule


// online

class LTLFormulaEvaluatorOnline(rule: Rule) extends Logging {
  private var formula = rule.formula.mapChildren { x => x }
  
  private var subFormulasList = ArrayBuffer[Formula](True, False)
  private var resolveList = Array[Formula]()
  private var deriveList = Array[Formula]()
  
  private var lastFormula :Formula= rule.formula.mapChildren { x => x }
  
  private var lastResolveList = Array[Formula]()
  private var lastDeriveList = Array[Formula]()
  
  private var trace :List[QueryModel] = List()
  
  private var isInit = false
  
  private def init(){
    findSubFormulas(formula)
//    LTLFormulaEvaluatorOnline.allSubFormulasList(rule.name) = subFormulasList
    resolveList =new Array[Formula](subFormulasList.size)
    deriveList =new Array[Formula](subFormulasList.size)
    lastResolveList =new Array[Formula](subFormulasList.size)
    lastDeriveList =new Array[Formula](subFormulasList.size)
  }
  
  private def findSubFormulas(formula: Formula){
      formula match {
        case Not(child) =>
          findSubFormulas(child)
          if (!subFormulasList.contains(formula)) subFormulasList.append(formula)

        case And(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(formula)

        case Or(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(formula)

        case Imply(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(formula)

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          findSubFormulas(x.child)
          val upper = x.upper
          val lower = x.lower
          for(i<- 0 to upper-lower){     
            val tmpX = x.mapChildren { x => x }.asInstanceOf[X]
            tmpX.lower = 0
            tmpX.upper = i
            if (!subFormulasList.contains(tmpX)) subFormulasList.append(tmpX)
          }
          for(i<- 0 to lower){
            val tmpX = x.mapChildren { x => x }.asInstanceOf[X]
            tmpX.lower = i
            tmpX.upper = i+upper-lower
            if (!subFormulasList.contains(tmpX)) subFormulasList.append(tmpX)

          }


        case _: U =>
          val u = formula.asInstanceOf[U]
          findSubFormulas(u.left)
          findSubFormulas(u.right)
          val upper = u.upper
          val lower = u.lower
          for(i<- 0 to upper-lower){             
            val tmpU = u.mapChildren { x => x }.asInstanceOf[U]
            tmpU.lower = 0
            tmpU.upper = i
            if (!subFormulasList.contains(tmpU)) subFormulasList.append(tmpU)

          }
          for(i<- 0 to lower){
            val tmpU = u.mapChildren { x => x }.asInstanceOf[U]
            tmpU.lower = i
            tmpU.upper = i+upper-lower
            if (!subFormulasList.contains(tmpU)) subFormulasList.append(tmpU)
          }
          
        
        case _: pX =>
          val px = formula.asInstanceOf[pX]
          findSubFormulas(px.child)
          val upper = px.upper
          val lower = px.lower
          for(i<- 0 to upper-lower){     
            val tmpPX = px.mapChildren { x => x }.asInstanceOf[pX]
            tmpPX.lower = 0
            tmpPX.upper = i
            if (!subFormulasList.contains(tmpPX)) subFormulasList.append(tmpPX)
          }
          for(i<- 0 to lower){
            val tmpPX = px.mapChildren { x => x }.asInstanceOf[pX]
            tmpPX.lower = i
            tmpPX.upper = i+upper-lower
            if (!subFormulasList.contains(tmpPX)) subFormulasList.append(tmpPX)
          }
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          findSubFormulas(pu.left)
          findSubFormulas(pu.right)
          val upper = pu.upper
          val lower = pu.lower
          for(i<- 0 to upper-lower){     
            val tmpPU = pu.mapChildren { x => x }.asInstanceOf[pU]
            tmpPU.lower = 0
            tmpPU.upper = i
            if (!subFormulasList.contains(tmpPU)) subFormulasList.append(tmpPU)
          }
          for(i<- 0 to lower){
            val tmpPU = pu.mapChildren { x => x }.asInstanceOf[pU]
            tmpPU.lower = i
            tmpPU.upper = i+upper-lower
            if (!subFormulasList.contains(tmpPU)) subFormulasList.append(tmpPU)
          }
          
        case _ =>
          if (!subFormulasList.contains(formula)) subFormulasList.append(formula)

      }
  }
  
  def monitor(model: QueryModel): Formula = {
    if (!isInit){
      init()
      isInit = true
    }
    
    trace = trace :+ model

    
    println("---initial:subFormulasList-----"+subFormulasList)
    println("---initial:resolveList-----"+resolveList.toBuffer)
    println("---initial:deriveList-----"+deriveList.toBuffer)
    println("---initial:lastresolveList-----"+lastResolveList.toBuffer)
    println("---initial:lastderiveList-----"+lastDeriveList.toBuffer)
    println("---initial:formula-----"+formula)
    println("---initial:lastFormula-----"+lastFormula)
    var i = trace.length-2
    println("---index:-------:"+i)
    
    if(i<0){
      i = 0
      for (k<-0 until subFormulasList.length){
        resolveList(k) = resolve(subFormulasList(k), trace, i)
      }
      for (k<-0 until subFormulasList.length){
        deriveList(k) = derive(subFormulasList(k), trace, i)
      }
      formula =  deriveList(subFormulasList.indexOf(formula))
      
      val simplifyAnalyzers = FormulaSimplifier()
      
      if (formula == True || formula == False) {
        return formula
      }
      else{
        return Unknown
      }
      
    }
    
    resolveList = lastResolveList.clone()
    deriveList = lastDeriveList.clone()

    formula = lastFormula.mapChildren { x => x}
    
    for (k<-0 until subFormulasList.length){
      resolveList(k) = resolve(subFormulasList(k), trace, i)
    }
      
    for (k<-0 until subFormulasList.length){
      deriveList(k) = derive(subFormulasList(k), trace, i)
    }
    
    lastResolveList = resolveList.clone()
    lastDeriveList = deriveList.clone()

    println("bug---------!!!!!!---"+formula+"^^^^^^"+subFormulasList)
    formula =  deriveList(subFormulasList.indexOf(formula))
      
    val simplifyAnalyzers = FormulaSimplifier()
    formula = simplifyAnalyzers.simplifyTorF(formula)  
    
    lastFormula = formula.mapChildren { x => x }

      
    if (formula == True || formula == False) {
      return formula
    }
    
    
    i += 1   
    
    for (k<-0 until subFormulasList.length){
      resolveList(k) = resolve(subFormulasList(k), trace, i)
    }
      
    for (k<-0 until subFormulasList.length){
      deriveList(k) = derive(subFormulasList(k), trace, i)
    }

    formula =  deriveList(subFormulasList.indexOf(formula))
    formula = simplifyAnalyzers.simplifyTorF(formula)  
    
    if (formula == True || formula == False) {
      return formula
    }
    else{
      return Unknown
    }
  }
  
  private def resolve(formula: Formula,trace: Seq[QueryModel], index: Int): Formula = {
    
    val result =
      formula match {
        case Not(child) =>
          Not(resolve(child, trace, index))

        case And(left, right) =>
          And(resolve(left, trace, index), resolve(right, trace, index))

        case Or(left, right) =>
          Or(resolve(left, trace, index), resolve(right, trace, index))

        case Imply(left, right) =>
          Or(Not(resolve(left, trace, index)), resolve(right, trace, index))

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
            resolveList(subFormulasList.indexOf(deriveList(subFormulasList.indexOf(px.child))))
          }
        
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          val upper = pu.upper
          val lower = pu.lower
          var formula1 :Formula= null
          var formula2 :Formula= null
          if(0 <= upper && 0 >= lower) {
            formula2 = resolveList(subFormulasList.indexOf(pu.right))
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
              if(pu.lower<0) pu.lower = 0
              formula1 = And(resolveList(subFormulasList.indexOf(pu.left)), resolveList(subFormulasList.indexOf(deriveList(subFormulasList.indexOf(pu))))) 
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
  
  private def derive(formula: Formula,trace: Seq[QueryModel], index: Int): Formula = {
        
    val result =
      formula match {
        case Not(child) =>
          Not(derive(child, trace, index))

        case And(left, right) =>
          And(derive(left, trace, index), derive(right, trace, index))

        case Or(left, right) =>
          Or(derive(left, trace, index), derive(right, trace, index))

        case Imply(left, right) =>
          Or(Not(derive(left, trace, index)), derive(right, trace, index))

        case True => True

        case False => False

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          val upper = x.upper
          val lower = x.lower
          if (index == trace.size-1 
              || !(trace(index+1).timestamp <= upper && trace(index+1).timestamp >= lower)){
//             if (index == trace.size-1 
//              || !(trace(index+1).timestamp <= upper && trace(index+1).timestamp >= lower)){
            False
          }
          else {
            x.child
          }

        case _: U =>
          val u = formula.asInstanceOf[U]
          println("&&&^^^^^"+u)
          val upper = u.upper
          val lower = u.lower
          var formula1 :Formula= null
          var formula2 :Formula= null
          if(0 <= upper && 0 >= lower) {
            formula1 = deriveList(subFormulasList.indexOf(u.right))
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
            println("time-----"+trace(index+1).timestamp)
            if (0 <= (upper - timeSubtract)){
              u.upper = upper - timeSubtract
              u.lower = lower - timeSubtract
              if(u.lower<0) u.lower = 0
              formula2 = And(deriveList(subFormulasList.indexOf(u.left)), u) 
              
            }
            else{
              formula2 = False
            }
          }
          Or(formula1, formula2)
        
        case _: pX =>
          //有问题
          deriveList(subFormulasList.indexOf(resolveList(subFormulasList.indexOf(formula))))
          
        case _: pU =>
          //有问题
          deriveList(subFormulasList.indexOf(resolveList(subFormulasList.indexOf(formula))))
          
          
        case _ =>
          val modelEvaluator = new FormulaEvaluator(trace(index), true)
          val tmpresult = modelEvaluator.evaluate(formula)
          if (tmpresult) True
          else False
    }
      
    result
  } 

}