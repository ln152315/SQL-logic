package edu.thu.ss.logic.evaluate

import edu.thu.ss.logic.SQLTest
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.types.IntegralType
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.dsl.expressions._
import edu.thu.ss.logic.paser.PolicyParser


class LTLFormulaEvaluatorOnlineSuite extends SQLTest {

  val parser = new PolicyParser
  

  test("test1") {
    val query1 = sqlContext.sql("select cid+aid id, name from customer")

    val plan1 = query1.queryExecution.analyzed
    val model1 = QueryModel.fromQueryPlan(plan1)
    model1.timestamp = 10
    
    val query2 = sqlContext.sql("select name from customer")

    val plan2 = query2.queryExecution.analyzed
    val model2 = QueryModel.fromQueryPlan(plan2)
    model1.timestamp = 30
    
    val query3 = sqlContext.sql("select aid, name from customer")

    val plan3 = query3.queryExecution.analyzed
    val model3 = QueryModel.fromQueryPlan(plan3)
    model1.timestamp = 33
    
    val trace = Seq(model1, model2, model3)
    val evaluator = new LTLFormulaEvaluatorOnline(trace)
    
    val policy = parser.parsePolicy("policy/policy1")
    
    
    policy.rules.foreach { rule =>
      {
        val value = evaluator.monitor(rule)
        println(s"$rule = $value")
        println(rule.formula.treeString)
      }
    }

  }

}

