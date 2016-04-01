package edu.thu.ss.logic.example

import edu.thu.ss.logic.definition.ISort
import edu.thu.ss.logic.definition.IFunction
import edu.thu.ss.logic.definition.IPredicate
import edu.thu.ss.logic.formula.Sort
import edu.thu.ss.logic.util.LogicUtils

class IntSort extends ISort[Int] {
  val valueClass = classOf[Int]

  def validInput(input: String): Boolean = LogicUtils.isInt(input)

  def parseInput(value: String): Int = value.toInt

  def validValue(value: Int): Boolean = true
}

class ColumnSort extends ISort[String] {
  private val columns = Set("c1", "c2", "c3", "c4", "c5")

  val valueClass = classOf[String]

  def validInput(value: String): Boolean = columns.contains(value)

  def parseInput(value: String): String = value

  def validValue(value: String): Boolean = columns.contains(value)

  override val finite = true

  override val values = columns
}

class Add extends IFunction {
  def evaluate(x: Int, y: Int): Int = x + y
}

class IsZero extends IPredicate {
  def evaluate(x: Int): Boolean = x == 0

  override def finite(index: Int) = true

  override def values(index: Int, otherParams: Seq[Any]): Seq[Any] = {
    Seq(0)
  }

}

class Equals extends IPredicate {
  def evaluate(x: Int, y: Int): Boolean = x == y

  override def finite(index: Int) = true

  override def values(index: Int, otherParams: Seq[Any]): Seq[Any] = {
    assert(otherParams.length == 1)
    otherParams
  }
}

class IsTrue extends IPredicate {
  def evaluate(value: Boolean): Boolean = value == true
}

class Output extends IPredicate {
  def evaluate(column: String): Boolean = {
    true
  }

}

class IsNumerical extends IPredicate {
  def evaluate(column: String): Boolean = column == "c1"

  override def finite(index: Int) = true

  override def values(index: Int, otherParams: Seq[Any]) = Seq("c1")
}
