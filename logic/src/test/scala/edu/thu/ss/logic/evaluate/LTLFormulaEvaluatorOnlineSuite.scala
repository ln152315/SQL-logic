package edu.thu.ss.logic.evaluate

import edu.thu.ss.logic.SQLTest
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.types.IntegralType
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.dsl.expressions._
import edu.thu.ss.logic.paser.PolicyParser
import scala.collection.mutable.ListBuffer


class LTLFormulaEvaluatorOnlineSuite extends SQLTest {

  val parser = new PolicyParser
  

  test("test1") {
    LogicChecker.init(sqlContext)

    
    val policy = parser.parsePolicy("policy/policy1")
    var evaluators : ListBuffer[LTLFormulaEvaluatorOnline] =new ListBuffer()
    
    policy.rules.foreach { rule =>
      {
        evaluators += new LTLFormulaEvaluatorOnline(rule)
        
      }
    }
    val query1 = sqlContext.sql("select address.aid from customer join address on customer.aid=address.aid")
    
    val plan1 = query1.queryExecution.analyzed
    val model1 = QueryModel.fromQueryPlan(plan1)
    model1.preprocess(model1.initialState, model1.finalState)
    model1.timestamp = 10
    
    evaluators.foreach { e =>{
      val value = e.monitor(model1)
      val rule = e.name
      println(s"$rule = $value")
      }
    }
    
    
    val query2 = sqlContext.sql("select na from (select concat(address.state, address.city) as na, address.aid as id from address ) as t1")

    val plan2 = query2.queryExecution.analyzed
    val model2 = QueryModel.fromQueryPlan(plan2)
    model2.preprocess(model2.initialState, model2.finalState)
    model2.timestamp = 12

    evaluators.foreach { e =>{
      val value = e.monitor(model2)
      val rule = e.name
      println(s"$rule = $value")
      }
    }
    
    val query3 = sqlContext.sql("select customer.name, first(customer.age) from customer group by customer.name, customer.salary")

    val plan3 = query3.queryExecution.analyzed
    val model3 = QueryModel.fromQueryPlan(plan3)
    model3.preprocess(model3.initialState, model3.finalState)
    model3.timestamp = 13
    
    
    evaluators.foreach { e =>{
      val value = e.monitor(model3)
      val rule = e.name
      println(s"$rule = $value")
      }
    }
    

    
    

  }

}

