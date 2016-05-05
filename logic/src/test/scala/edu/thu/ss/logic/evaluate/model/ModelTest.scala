package edu.thu.ss.logic.evaluate.model

import edu.thu.ss.logic.SQLTest
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.LogicChecker
import org.apache.spark.sql.catalyst.expressions.AttributeReference
import org.apache.spark.sql.types.IntegralType
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.dsl.expressions._
import edu.thu.ss.logic.sql.{SqlFieldsParser, SlickGenerator}
import edu.thu.ss.logic.sql.StringCamelImplicits
import org.apache.spark.sql.DataFrame

import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.types._;
import org.apache.spark.sql.Row
class ModelTest extends SQLTest {
  
  test("test1") {
    
    
//    val querytest = sqlContext.sql("CREATE TABLE dbgen_version(dv_version        VARCHAR(16),dv_create_date    timestamp,dv_create_time    TIMESTAMP,dv_cmdline_args   VARCHAR(200))")

//    val querytest = sqlContext.sql("CREATE TEMPORARY TABLE test (key INT) USING org.apache.spark.sql.avro OPTIONS (path '../hive/src/test/resources/data/files/episodes.avro')")
//    val plantest = querytest.queryExecution.analyzed
//    val query = sqlContext.sql("select sum(count(customer.cid)) from customer join address where customer.aid>=address.aid")
////    val query = sqlContext.sql("select sum(customer.cid) from customer join address where customer.aid>=address.aid")
//    val query = sqlContext.sql("select customer.cid from customer union select customer.age from customer union select address.aid from address ")
//    val query = sqlContext.sql("select customer.cid, customer.name from customer order by customer.name, customer.cid DESC")
    val query = sqlContext.sql("select customer.name, first(customer.age) from customer group by customer.name, customer.salary")
//    val query = sqlContext.sql("select customer.name as id from customer ")
    val plan = query.queryExecution.analyzed
    val model = QueryModel.fromQueryPlan(plan)
    model.preprocessAccess(sqlContext)
    println(model.getTables)
//
//    println("------model:"+model)
//
//    val id = model.finalState.plan.output.find { _.name == "name" }.get
//    println("------id:"+model.finalState.getBaseAttributes(id))
////
//    LogicChecker.init(sqlContext)
//
//    val tables = LogicChecker.getDatabaseSchema("")
//    println("------tables:"+tables)
//    println("---state:"+state+"-----attr:-"+state.attributeExprs)


  }
}