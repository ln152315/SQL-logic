package edu.thu.ss.logic.model

import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.SQLContext
import edu.thu.ss.logic.tree.DoubleNode
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import scala.collection.mutable.ListBuffer
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import org.apache.spark.sql.catalyst.plans.logical.BinaryNode
import org.apache.spark.sql.catalyst.plans.logical.UnaryNode
import org.apache.spark.sql.catalyst.plans.logical.LeafNode
import scala.collection.mutable
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.catalyst.expressions.Attribute
import org.apache.spark.sql.catalyst.expressions.Expression
import org.apache.spark.sql.catalyst.plans.logical.Project
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.catalyst.expressions.Alias
import org.apache.spark.sql.catalyst.expressions.NamedExpression
import org.apache.spark.sql.catalyst.plans.logical.Aggregate
import org.apache.spark.sql.catalyst.plans.logical.Filter
import org.apache.spark.sql.catalyst.plans.logical.Union
import edu.thu.ss.logic.formula.Formula
import java.util.Date

abstract class State extends DoubleNode[State] {
  var parent: State = null

  def nodeName = "state"

  def plan: LogicalPlan

  private[model] val attributeExprs = new mutable.HashMap[Attribute, Expression]

  private[model] val attributeMap = new mutable.HashMap[Attribute, Seq[Attribute]]

  private val formulaCache = new mutable.HashMap[Formula, Boolean]

  def getAttribute(name: String): Attribute =
    plan.output.find { _.name == name } match {
      case Some(a) => a
      case None => throw new IllegalArgumentException(s"$name is not output by the current plan operator")
    }

  def getAttributeDefinition(attr: String): Expression =
    getAttributeDefinition(getAttribute(attr))

  def getBaseAttribute(attr: String): Seq[Attribute] =
    getBaseAttributes(getAttribute(attr))

  def getBaseAttributes(attr: Attribute): Seq[Attribute] = {
    attributeMap.getOrElseUpdate(attr, {
      getAttributeDefinition(attr).collect({
        case attribute: Attribute => attribute
      })
    })
  }

  def getAttributeDefinition(attr: Attribute): Expression =
    attributeExprs.get(attr) match {
      case Some(e) => e
      case None => throw new IllegalArgumentException(s"$attr is not output by the current plan operator")
    }

  def clearCache {
    formulaCache.clear
  }

  def cacheFormula(formula: Formula, value: Boolean) = {
    formulaCache.put(formula, value)
    value
  }

  def getFormula(formula: Formula) = formulaCache.get(formula)

}

case class BinaryState(left: State, right: State, plan: BinaryNode) extends State {

  val children = left :: right :: Nil

}

case class UnaryState(child: State, plan: UnaryNode) extends State {

  val children = child :: Nil
}

case class LeafState(plan: LeafNode) extends State {

  val children = Nil

}

case class QueryModel(initialStates: Seq[State], finalState: State) {
  var timestamp = 0.toInt
  
//  private var global = new mutable.HashMap[String, String]
  
  private var role :String = null
  
  private var purpose :String = null
  
  private var tables :ListBuffer[String] = new ListBuffer() 
  
//  private var union :Set[(String, String)] = new Set()
  
  def clearCache {
    finalState.foreach { _.clearCache }

  }
  
  def setRole(role: String){
    this.role = role
//    global.put("Role", role)
  }
  
  def setPurpose(purpose: String){
    this.purpose = purpose
//    global.put("Purpose", purpose)
  }
  
//  def getGlobal = global
  
  
  def getTables = tables
 
  def preprocessAccess(sqlContext: SQLContext) {
    // accessed tables
    LogicChecker.init(sqlContext)
    var alltables = LogicChecker.getDatabaseSchema("")
    alltables.foreach{f => initialStates.foreach { x => {
      if(f._2.equals(x.plan.schema))
        tables += f._1
      } }}
  }
  
  def preprocessUnion{
    
  }
  
  
  def global = (role, purpose, tables)

}

object QueryModel {
  val Empty = {
    val state = new LeafState(null)

    new QueryModel(Seq(state), state)
  }

  def fromQueryPlan(plan: LogicalPlan): QueryModel = {
    val initialStates = new ListBuffer[State]
    val finalState = translatePlan(plan, initialStates)
    buildAttributeExprs(finalState)
    new QueryModel(initialStates, finalState)
  }

  private def translatePlan(plan: LogicalPlan, initialStates: ListBuffer[State]): State = {
    plan match {
      case binary: BinaryNode =>
        val left = translatePlan(binary.left, initialStates)
        val right = translatePlan(binary.right, initialStates)
        val state = BinaryState(left, right, binary)
        left.parent = state
        right.parent = state
        state
      case unary: UnaryNode =>
        val child = translatePlan(unary.child, initialStates)
        val state = UnaryState(child, unary)
        child.parent = state
        state
      case leaf: LeafNode =>
        println("LocalRelation:")
        val state = LeafState(leaf)
        initialStates.append(state)
        state
    }
  }

  private def buildAttributeExprs(finalState: State) {
    finalState.foreachUp { state =>
      state.attributeExprs.clear
      state.plan match {
        case sort: org.apache.spark.sql.catalyst.plans.logical.Sort =>
          println("Order:"+ sort.order(0).child)
          println("Order:"+ sort.order(0).direction)
          println("Order:"+ sort.order.exists { x => (x.child.toString() == "name#1")&&(x.direction.toString() == "Ascending") })
        case u: Union =>
          println()
        case f: Filter =>
          println("Filter:"+ f.condition.containsChild.exists { _.toString() == "aid#5" })
        case leaf: LeafNode =>
          //initialization   
          leaf.output.foreach { attr => state.attributeExprs.put(attr, attr) }
        case proj: Project =>
          println("Project:" + proj.output)
//          println(proj.output.exists { _.toString() == "cid#0" })
          buildTransformation(state, proj.projectList, state.children(0))
        case agg: Aggregate =>
          println("Aggregate:"+agg.groupingExpressions.exists { x => x.toString() == "name#1" })
          agg.aggregateExpressions.foreach { x => {
//            println(x.nodeName)
            println(x.children)
            
            x.containsChild.foreach { c => println(c.nodeName) }
            x.containsChild.foreach { c => c.containsChild.foreach { x => println(x.containsChild) } }
          }
          
//          println(agg.aggregateExpressions.exists { x => {
//          x.nodeName.equals("Alias") && x.containsChild.exists { c => {
//            c.nodeName.equals("First") && (c.containsChild.exists { a => a.toString().equals("age#2") } || c.containsChild.exists { a => a.containsChild.exists { b => b.toString().equals("age#2") } } )
//
//            }
////           
//          }
//        } })
             }
          println("Aggregate:"+agg.child)

          buildTransformation(state, agg.aggregateExpressions, state.children(0))
        case _ => state.children.foreach { state.attributeExprs ++= _.attributeExprs }
      }

    }
  }

  private def buildTransformation(state: State, attrList: Seq[NamedExpression], child: State) {
    attrList.foreach { expr =>
      expr match {
        case ref: Attribute => state.attributeExprs.put(ref, ref)
        case alias: Alias =>
          val transformed = alias.child.transform {
            case attr: Attribute => child.attributeExprs.getOrElse(attr, null)
          }
          state.attributeExprs.put(alias.toAttribute, transformed)
      }
    }
  }

}