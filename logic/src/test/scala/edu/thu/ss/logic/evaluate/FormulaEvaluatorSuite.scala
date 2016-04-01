package edu.thu.ss.logic.evaluate

import org.scalatest.FunSuite
import edu.thu.ss.logic.paser.PolicyParser
import edu.thu.ss.logic.model.QueryModel

class FormulaEvaluatorSuite extends FunSuite {

  val parser = new PolicyParser
  val evaluator = new FormulaEvaluator(QueryModel.Empty, true)

  test("test1") {

    val policy = parser.parsePolicy("policy/policy1")

    policy.rules.foreach { rule =>
      {
        val value = evaluator.evaluate(rule)
        println(s"$rule = $value")
        println(rule.formula.treeString)
      }
    }

  }

}

