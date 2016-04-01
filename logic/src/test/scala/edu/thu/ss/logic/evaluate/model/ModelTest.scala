package edu.thu.ss.logic.evaluate.model

import edu.thu.ss.logic.SQLTest
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.types.IntegralType
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.dsl.expressions._

class ModelTest extends SQLTest {

  test("test1") {
    val query = sqlContext.sql("select cid+aid id, name from customer")

    val plan = query.queryExecution.analyzed
    val model = QueryModel.fromQueryPlan(plan)

    println(model)

    val id = model.finalState.plan.output.find { _.name == "id" }.get
    println(model.finalState.getBaseAttributes(id))

    LogicChecker.init(sqlContext)

    val tables = LogicChecker.getDatabaseSchema("")
    println(tables)

  }
}