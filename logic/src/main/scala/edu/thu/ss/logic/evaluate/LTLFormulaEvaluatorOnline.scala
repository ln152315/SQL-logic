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
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import edu.thu.ss.logic.policy.Rule


// online

class LTLFormulaEvaluatorOnline(rule: Rule) extends Logging {
  var name = rule.name
  
  
  
  private var subFormulasList = ArrayBuffer[Formula](True, False)
  
  private var formula = rule.formula
  
  private var resolveList = Array[Formula]()
  private var deriveList = Array[Formula]()
  
  private var lastFormula :Formula= rule.formula
  
  private var lastResolveList = Array[Formula]()
  private var lastDeriveList = Array[Formula]()
  
  private var trace :ListBuffer[QueryModel] =new ListBuffer()
  
  private var isInit = false
  
  private def init(){   
    findSubFormulas(formula)
    resolveList =new Array[Formula](subFormulasList.size)
    deriveList =new Array[Formula](subFormulasList.size)
    lastResolveList =new Array[Formula](subFormulasList.size)
    lastDeriveList =new Array[Formula](subFormulasList.size)
    
  }
  
  private def findSubFormulas(formula: Formula){
      formula match {
        case Not(child) =>
          findSubFormulas(child)
          if (!subFormulasList.contains(formula)) subFormulasList.append(Not(child))

        case And(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(And(left, right))

        case Or(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(Or(left, right))

        case Imply(left, right) =>
          findSubFormulas(left)
          findSubFormulas(right)
          if (!subFormulasList.contains(formula)) subFormulasList.append(Imply(left, right))

        //temporal part
        case _: X =>
          val x = formula.asInstanceOf[X]
          findSubFormulas(x.child)
          val upper = x.upper
          val lower = x.lower
          for(i<- 0 to upper-lower){     
            val tmpX = X(Seq(0.toString(), i.toString()), x.child)
            if (!subFormulasList.contains(tmpX)) subFormulasList.append(tmpX)
          }
          for(i<- 0 to lower){
            val tmpX = X(Seq(i.toString(), (i+upper-lower).toString()), x.child)
            if (!subFormulasList.contains(tmpX)) subFormulasList.append(tmpX)

          }


        case _: U =>
          val u = formula.asInstanceOf[U]
          findSubFormulas(u.left)
          findSubFormulas(u.right)
          val upper = u.upper
          val lower = u.lower
          for(i<- 0 to upper-lower){             
            val tmpU = U(Seq(0.toString(), i.toString()), u.left, u.right)

            if (!subFormulasList.contains(tmpU)) subFormulasList.append(tmpU)

          }
          for(i<- 0 to lower){
            val tmpU = U(Seq(i.toString(), (i+upper-lower).toString()), u.left, u.right)
            if (!subFormulasList.contains(tmpU)) subFormulasList.append(tmpU)
          }
          
        
        case _: pX =>
          val px = formula.asInstanceOf[pX]
          findSubFormulas(px.child)
          val upper = px.upper
          val lower = px.lower
          for(i<- 0 to upper-lower){     
            val tmpPX = pX(Seq(0.toString(), i.toString()), px.child)
            if (!subFormulasList.contains(tmpPX)) subFormulasList.append(tmpPX)
          }
          for(i<- 0 to lower){
            val tmpPX = pX(Seq(i.toString(), (i+upper-lower).toString()), px.child)
            if (!subFormulasList.contains(tmpPX)) subFormulasList.append(tmpPX)
          }
                
        case _: pU =>
          val pu = formula.asInstanceOf[pU]
          findSubFormulas(pu.left)
          findSubFormulas(pu.right)
          val upper = pu.upper
          val lower = pu.lower
          for(i<- 0 to upper-lower){     
            val tmpPU = pU(Seq(0.toString(), i.toString()), pu.left, pu.right)
            if (!subFormulasList.contains(tmpPU)) subFormulasList.append(tmpPU)
          }
          for(i<- 0 to lower){
            val tmpPU = pU(Seq(i.toString(), (i+upper-lower).toString()), pu.left, pu.right)
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
    
    trace += model

    
//    println("---initial:subFormulasList-----"+subFormulasList)
//    println("---initial:resolveList-----"+resolveList.toBuffer)
//    println("---initial:deriveList-----"+deriveList.toBuffer)
//    println("---initial:lastresolveList-----"+lastResolveList.toBuffer)
//    println("---initial:lastderiveList-----"+lastDeriveList.toBuffer)
//    println("---initial:formula-----"+formula)
//    println("---initial:lastFormula-----"+lastFormula)
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
      
//      println("---0:subFormulasList-----"+subFormulasList)
//      println("---0:resolveList-----"+resolveList.toBuffer)
//      println("---0:deriveList-----"+deriveList.toBuffer)
//      println("---0:lastresolveList-----"+lastResolveList.toBuffer)
//      println("---0:lastderiveList-----"+lastDeriveList.toBuffer)
//      println("---0:formula-----"+formula)
//      println("---0:lastFormula-----"+lastFormula)
      
      if (formula == True || formula == False) {
        return formula
      }
      else{
        return Unknown
      }
      
    }
//    println("---1:subFormulasList-----"+subFormulasList)
//    println("---1:resolveList-----"+resolveList.toBuffer)
//    println("---1:deriveList-----"+deriveList.toBuffer)
//    println("---1:lastresolveList-----"+lastResolveList.toBuffer)
//    println("---1:lastderiveList-----"+lastDeriveList.toBuffer)
//    println("---1:formula-----"+formula)
//    println("---1:lastFormula-----"+lastFormula)
    resolveList = lastResolveList.clone()
    deriveList = lastDeriveList.clone()

    formula = lastFormula
//    println("---2:subFormulasList-----"+subFormulasList)
//    println("---2:resolveList-----"+resolveList.toBuffer)
//    println("---2:deriveList-----"+deriveList.toBuffer)
//    println("---2:lastresolveList-----"+lastResolveList.toBuffer)
//    println("---2:lastderiveList-----"+lastDeriveList.toBuffer)
//    println("---2:formula-----"+formula)
//    println("---2:lastFormula-----"+lastFormula)
    
    
    for (k<-0 until subFormulasList.length){
      resolveList(k) = resolve(subFormulasList(k), trace, i)
    }
      
    for (k<-0 until subFormulasList.length){
      deriveList(k) = derive(subFormulasList(k), trace, i)
    }
    
    lastResolveList = resolveList.clone()
    lastDeriveList = deriveList.clone()

    formula =  deriveList(subFormulasList.indexOf(formula))
    
      
    val simplifyAnalyzers = FormulaSimplifier()
    formula = simplifyAnalyzers.simplifyTorF(formula)  
    
    lastFormula = formula
    
//    println("---3:subFormulasList-----"+subFormulasList)
//    println("---3:resolveList-----"+resolveList.toBuffer)
//    println("---3:deriveList-----"+deriveList.toBuffer)
//    println("---3:lastresolveList-----"+lastResolveList.toBuffer)
//    println("---3:lastderiveList-----"+lastDeriveList.toBuffer)
//    println("---3:formula-----"+formula)
//    println("---3:lastFormula-----"+lastFormula)
      
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
    
//    println("---4:subFormulasList-----"+subFormulasList)
//    println("---4:resolveList-----"+resolveList.toBuffer)
//    println("---4:deriveList-----"+deriveList.toBuffer)
//    println("---4:lastresolveList-----"+lastResolveList.toBuffer)
//    println("---4:lastderiveList-----"+lastDeriveList.toBuffer)
//    println("---4:formula-----"+formula)
//    println("---4:lastFormula-----"+lastFormula)
    
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
        case x: X =>
          X(x.interval, x.child)

        case u: U =>
          U(u.interval, u.left, u.right)
        
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
            val timeSubtract = trace(index).timestamp - trace(index-1).timestamp
            if (0 < (upper - timeSubtract)){
              
              var tmpLower = lower - timeSubtract
              var tmpUpper = upper - timeSubtract
              if(tmpLower<0) tmpLower = 0
              val tmpPU = pU(Seq(tmpLower.toString(), tmpUpper.toString()), pu.left, pu.right)
              
              formula1 = And(resolveList(subFormulasList.indexOf(pu.left)), resolveList(subFormulasList.indexOf(deriveList(subFormulasList.indexOf(pU))))) 
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
          if (index == trace.size-1){
            Unknown
          }
          else if(!(trace(index+1).timestamp <= upper && trace(index+1).timestamp >= lower)){
            False
          }
          else {
            x.child
          }

        case _: U =>
          val u = formula.asInstanceOf[U]
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
            formula2 = Unknown
          }
          else{
            val timeSubtract = trace(index+1).timestamp - trace(index).timestamp
            if (0 < (upper - timeSubtract)){
              var tmpLower = lower - timeSubtract
              var tmpUpper = upper - timeSubtract
              if(tmpLower<0) tmpLower = 0
              val tmpU = U(Seq(tmpLower.toString(), tmpUpper.toString()), u.left, u.right)
              formula2 = And(deriveList(subFormulasList.indexOf(tmpU.left)), tmpU) 
              
            }
            else{
              formula2 = False
            }
          }
          Or(formula1, formula2)
        
        case _: pX =>
          deriveList(subFormulasList.indexOf(resolveList(subFormulasList.indexOf(formula))))
          
        case _: pU =>
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