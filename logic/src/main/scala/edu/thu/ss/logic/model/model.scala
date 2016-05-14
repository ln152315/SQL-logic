package edu.thu.ss.logic.model

import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.SQLContext
import edu.thu.ss.logic.tree.DoubleNode
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import scala.collection.mutable.ListBuffer
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
import org.apache.spark.sql.hive.MetastoreRelation
import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.types._;
import edu.thu.ss.logic.formula.Formula
import java.util.Date

abstract class State extends DoubleNode[State] {
  var parent: State = null
  
  var parents =new ListBuffer[State]
  
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

case class LeafState(child: State, plan: LeafNode) extends State {

  val children = child :: Nil

}

case class InitialState(plan: LeafNode) extends State {

  val children = Nil

}

case class QueryModel(initialState: State, finalState: State) {
  var timestamp = 0.toInt
  
//  private var global = new mutable.HashMap[String, String]
  private var role :String = null
  
  private var purpose :String = null
  
  private var sqlText :String = null
  
  private var tables :ListBuffer[String] = new ListBuffer() 
  
  private var columns:ListBuffer[(String, String)] = new ListBuffer()
  
  private var columnsExpr = new mutable.HashMap[String, String]
  
  private var alias = new mutable.HashMap[String, Set[String]]
  
  private var unions :Set[(String, String)] = Set()
  
  private var joins :Set[(String, String)] = Set()
    
  def clearCache {
    finalState.foreach { _.clearCache }

  }
  
  def setSQLText(sqlText: String){
    this.sqlText = sqlText
  }
  
  def setRole(role: String){
    this.role = role
  }
  
  def setPurpose(purpose: String){
    this.purpose = purpose
  }
  

  def getRole = role
  
  def getPurpose = purpose
  
  def getSQLText = sqlText
  
  def getTables = tables
  
  def getUnions = unions
  
  def getJoins = joins
  
  def getAlias = alias
  
  def getColumnsExpr = columnsExpr
  
  def preprocess(initialState: State, finalState: State){
    preprocessAccess(initialState)
    pregrocessColumns()
    mapFromColumnToExpr(initialState)
    preprocessUnionOrJoin(finalState)
    println("Unions:"+unions)
    println("Joins:"+joins)
    println("Alias:"+alias)
    println("Tables:"+tables)
    println("Columns:"+columns)
    println("ColumnsExpr:"+columnsExpr)
  }
 
  def preprocessAccess(initialState: State) {
    // accessed tables   
    var alltables = LogicChecker.getDatabaseSchema("default")
    alltables.foreach{f => initialState.parents.foreach { x => {
      if(f._2.equals(x.plan.schema))
        tables += f._1
      } }}
    
  }
  
  def pregrocessColumns(){
    tables.foreach { table => 
      columns ++= LogicChecker.getColumnFromTable(table)
    }
  }
  
  
  
  def mapFromColumnToExpr(initialState: State){
    initialState.parents.foreach { x => 
      val lr = x.plan
      val tableName = getTableNameFromRelation(lr)
        if(tableName!=null){
          lr.output.foreach { c => columnsExpr(tableName+"."+c.name) = c.toString() }
        }
      }
  }
  
  def getTableNameFromRelation(lr: LogicalPlan): String ={
    // accessed tables   
    var alltables = LogicChecker.getDatabaseSchema("default")
    alltables.find{f => f._2.equals(lr.schema)
      } match{
        case Some(s) => s._1
        case None => null
      }
    
  }
  
  def preprocessUnionOrJoin(state: State): ListBuffer[StructType] = {
    var leftTable = new ListBuffer[StructType]
    var rightTable = new ListBuffer[StructType]
    state.plan match{
      case f: Filter =>
        println("Filter:"+f)
        preprocessUnionOrJoin(state.asInstanceOf[UnaryState].child)
//      case m: MetastoreRelation =>
//        println("LocalRelation")
//        ListBuffer(m.schema)
      case r: LocalRelation => 
        println("LocalRelation")
        ListBuffer(r.schema)
      case u: Union => 
        leftTable = preprocessUnionOrJoin(state.asInstanceOf[BinaryState].left)
        rightTable = preprocessUnionOrJoin(state.asInstanceOf[BinaryState].right)
        
        leftTable.foreach { l => rightTable.foreach { r => unions += getTableName(l, r) } }
        
        leftTable ++ rightTable
      case j: Join => 
        leftTable = preprocessUnionOrJoin(state.asInstanceOf[BinaryState].left)
        rightTable = preprocessUnionOrJoin(state.asInstanceOf[BinaryState].right)
        
        leftTable.foreach { l => rightTable.foreach { r => joins += getTableName(l, r) } }
        
        leftTable ++ rightTable
      case proj: Project => 
        proj.projectList.foreach { x => 
            x match{
              case a: Alias => 
                val key = a.name+"#"+a.exprId.id
                a.child.foreach { y => {
                  if (y.toString().split("#").length ==2 ){
                    if (!alias.contains(key)){
                      alias(key) = Set()
                    }
                    alias(key) += y.toString()
                  }
                  } 
                }
              case _ => null
            }
          }
        preprocessUnionOrJoin(state.asInstanceOf[UnaryState].child)
      
      case unary: UnaryNode =>
        println(unary.nodeName)
        preprocessUnionOrJoin(state.asInstanceOf[UnaryState].child)
      
      case binary: BinaryNode => 
        preprocessUnionOrJoin(state.asInstanceOf[BinaryState].left) ++ preprocessUnionOrJoin(state.asInstanceOf[BinaryState].right)
      
      case _ =>
        println(state.plan.nodeName)
        ListBuffer(state.plan.schema)
       
    }
    
    
  }
  
  def getTableName(left: StructType, right: StructType): (String, String) ={
    var alltables = LogicChecker.getDatabaseSchema("default")
    val leftName = alltables.find{ p => p._2.equals(left)} match{
      case None => null
      case Some(s) => s._1
    }
    val rightName = alltables.find{ p => p._2.equals(right)} match{
      case None => null
      case Some(s) => s._1
    }
    
    
    (leftName, rightName)
  }

  

}

object QueryModel {
  val Empty = {
    val state = new InitialState(null)

    new QueryModel(state, state)
  }


  def fromQueryPlan(plan: LogicalPlan): QueryModel = {
//    val initialStates = new ListBuffer[State]
//    val finalState = translatePlan(plan, initialStates)
//    buildAttributeExprs(finalState)
//    new QueryModel(initialStates, finalState)
    val initialState = new InitialState(null)
    val finalState = translatePlan(plan, initialState)
    buildAttributeExprs(finalState)
    new QueryModel(initialState, finalState)
  }

  private def translatePlan(plan: LogicalPlan, initialState: State): State = {
    plan match {
      case binary: BinaryNode =>
        val left = translatePlan(binary.left, initialState)
        val right = translatePlan(binary.right, initialState)
        val state = BinaryState(left, right, binary)
        left.parents += state
        right.parents += state
        state
      case unary: UnaryNode =>
        val child = translatePlan(unary.child, initialState)
        val state = UnaryState(child, unary)
        child.parents += state
        state
      case leaf: LeafNode =>
        val state = LeafState(initialState, leaf)
        initialState.parents += state
        state
//      case _ => 
//        val state = LeafState(initialState, _.asInstanceOf[LeafNode])
//        initialState.parents += state
//        state
    }
  }

  private def buildAttributeExprs(finalState: State) {
    finalState.foreachUp { state =>
      state.attributeExprs.clear
      state.plan match {
        case leaf: LeafNode =>
          //initialization   
          leaf.output.foreach { attr => state.attributeExprs.put(attr, attr) }
        case proj: Project =>
          buildTransformation(state, proj.projectList, state.children(0))
        case agg: Aggregate =>
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