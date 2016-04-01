package edu.thu.ss.logic

import org.apache.spark.sql.SQLContext
import edu.thu.ss.logic.policy.Policy
import edu.thu.ss.logic.formula.LogicDefinitions
import edu.thu.ss.logic.paser.PolicyParser
import org.apache.spark.sql.catalyst.plans.logical.LogicalPlan
import edu.thu.ss.logic.policy.Rule
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.evaluate.FormulaEvaluator
import org.apache.spark.sql.types.StructType

class LogicChecker private (val policy: Policy) {

  /**
   * returns a violated rule
   */
  def check(plan: LogicalPlan): Option[Rule] = {
    val model = QueryModel.fromQueryPlan(plan)
    val evaluator = new FormulaEvaluator(model, true)

    policy.rules.foreach { rule =>
      if (!evaluator.evaluate(rule.formula))
        return Some(rule)
    }
    return None
  }

}

object LogicChecker {
  private var sqlContext: SQLContext = null
  private var initialized = false

  def init(context: SQLContext) {
    this.sqlContext = context

    initialized = true
  }

  def getSQLContext: SQLContext = {
    if (!initialized) {
      throw new IllegalStateException("LogicCheck has not been initialized.")
    }
    sqlContext
  }

  def getTableSchema(table: String) = {
    sqlContext.table(table).schema
  }

  def getDatabaseSchema(database: String): Seq[(String, StructType)] = {
    val tables = sqlContext.tables(database).collect()
    tables.map { row =>
      val table = row.getString(0)
      (table, getTableSchema(table))
    }
  }

  def fromPolicy(path: String): LogicChecker = {
    val parser = new PolicyParser
    val policy = parser.parsePolicy(path)
    new LogicChecker(policy)
  }

}