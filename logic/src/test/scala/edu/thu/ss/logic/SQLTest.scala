package edu.thu.ss.logic

import org.scalatest.FunSuite
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.catalyst.dsl.expressions._
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.DataFrame

abstract class SQLTest extends FunSuite {
  lazy val sparkContext = SQLTest._sparkContext

  lazy val sqlContext = SQLTest._sqlContext

}

object SQLTest {
  lazy val _sparkContext = {
    val conf = new SparkConf
    conf.set("spark.app.name", "test")
    conf.set("spark.master", "local")
    new SparkContext(conf)
  }

  lazy val _sqlContext = {
    val context = new SQLContext(_sparkContext)
    val customer = LocalRelation('cid.int, 'name.string, 'age.int, 'salary.double, 'aid.int)
    new DataFrame(context, customer).registerTempTable("customer")
    val address = LocalRelation('aid.int, 'state.string, 'city.string, 'street.string, 'zip.string)
    new DataFrame(context, customer).registerTempTable("address")

    context
  }

}