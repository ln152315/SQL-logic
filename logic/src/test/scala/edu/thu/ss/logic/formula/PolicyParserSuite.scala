package edu.thu.ss.logic.formula

import scala.reflect.runtime.universe._
import scala.util.parsing.combinator.Parsers

import org.scalatest.FunSuite

import edu.thu.ss.logic.definition.ISort
import edu.thu.ss.logic.example.IntSort
import edu.thu.ss.logic.paser.PolicyParser

class PolicyParserTest extends FunSuite {

  val parser = new PolicyParser

  test("test1") {
    val policy = parser.parsePolicy("policy/policy1")
    println(policy)
  }

}