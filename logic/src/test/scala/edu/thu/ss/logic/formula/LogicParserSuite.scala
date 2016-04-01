package edu.thu.ss.logic.formula

import org.scalatest.FunSuite
import edu.thu.ss.logic.paser.LogicParser
import java.io.StringReader
import scala.util.parsing.combinator.Parsers
import java.io.FileReader

class LogicParserTest extends FunSuite {

  val parser = LogicParser
  implicit def toReader(str: String) = {
    new parser.lexical.Scanner(str)
  }
  
  test("test") {
    parseFormula("G[1,2] (True)");
  }

  test("test1") {
    parseFormula("G[100,200] (f1() AND f2(1))");
  }

  test("test2") {
    parseFormula("G[100,200] (f1() AND f2(1) AND f3(f1()))")
  }

  test("test3") {
    parseFormula("G[100,200] (f1() OR f2(1) AND f3(f1()))")
  }

  test("test associativity") {
    parseFormula("G[100,200] (f1() IMPLY f2() IMPLY f3() IMPLY f4())")

  }

  test("test ()") {
    parseFormula("G[100,200] ((f1() OR f2(1)) AND f3(f1()))")

  }
  
  test("test temporal1") {
    parseFormula("G[100,200] (AF ( f1() AND AF g1()))")

  }
  
  test("test temporal2") {
    parseFormula("G[100,200] ( X[100,300]( f1() AND AF g1()) )")

  }
  
  test("test temporal3") {
    parseFormula("(f1()) U[100,200] (f2() IMPLY f3() AND f4())")

  }
  
  test("test temporal4") {
    parseFormula("(f1() AND f2() IMPLY f3()) U[100,200] (f4())")

  }
  
  test("test temporal5") {
    parseFormula("G[100,200] ((f1() IMPLY f2() AU f3() AND f4()))")

  }
  
  test("test temporal6") {
    parseFormula("F[1,2] (f1() IMPLY f2() AU f3() AND f4()) IMPLY G(1,2) (f1()) U(2,3) (f1())")

  }
  
  //TODO 如果将z 改成x 就不行了
  test("test quantifier1") {
    parseFormula("G[100,200] (forall int z, int y. f1(z) AND g1(y))")

  }

  test("test quantifier2") {
    parseFormula("G[100,200] (forall int z. exists int y. f1(z) AND g1(y))")

  }

  test("test quantifier3") {
    parseFormula("G[100,200] ((forall int z. f1(z)) AND g1(y))")

  }

  test("test symbol") {
    parseFormula("G[100,200] (f1() && f2() || g1() -> f3())")

  }

  test("test sort") {
    parseDefinition("define sort int class=edu.thu.ss.logic.IntSort;")
  }
  test("test function") {
    parseDefinition("define function int add(int z, double y) class=edu.thu.ss.logic.Add;")
  }
  test("test predicate") {
    parseDefinition("define predicate isZero(int z) class=edu.thu.ss.logic.isZero;")
  }

//  test("test file") {
//    val (defs, rules) = parser.parseFile("policy/policy1")
//    defs.foreach(println(_))
//    rules.foreach(println(_))
//  }

  def parseFormula(str: String) {
    val result = parser.parseFormula(str)
    if (result.successful) {
      println(result.get)
    } else {
      fail(result.toString)
    }
  }

  def parseDefinition(str: String) {
    val result = parser.parseDef(str)
    if (result.successful) {
      println(result.get)
    } else {
      fail(result.toString)
    }
  }

}