package edu.thu.ss.logic.util

import scala.collection.mutable
import scala.reflect.runtime.universe._
import edu.thu.ss.logic.example.IntSort
import edu.thu.ss.logic.definition.ISort
import java.{ lang => java }
import edu.thu.ss.logic.evaluate.EvaluationContext
import edu.thu.ss.logic.formula._

object LogicUtils {

  val mirror = runtimeMirror(getClass.getClassLoader)

  private val numerics = List(classOf[Int], classOf[Short], classOf[Double], classOf[Float],
    classOf[Long], classOf[Byte], classOf[Boolean], classOf[java.Number], classOf[java.Boolean])

  def isNumericalValue(value: Any): Boolean = {
    numerics.exists { _.isAssignableFrom(value.getClass) }
  }

  private def isNumerical(value: String, test: String => Any): Boolean = {
    try {
      test(value)
      true
    } catch {
      case e: NumberFormatException => false
    }
  }

  def isNumerical(value: String): Boolean =
    isNumerical(value, _.toDouble)

  def isInt(value: String): Boolean =
    isNumerical(value, _.toInt)

  def checkUnique(seq: Seq[Any], toId: (Any) => String, onError: (Any) => Unit) {
    val set = new mutable.HashSet[String]
    seq.foreach(ele => {
      val id = toId(ele)
      if (set.contains(id)) {
        onError(ele)
      } else {
        set.add(id)
      }
    })
  }

  def sideBySide(left: String, right: String): Seq[String] = {
    sideBySide(left.split("\n"), right.split("\n"))
  }

  def sideBySide(left: Seq[String], right: Seq[String]): Seq[String] = {
    val maxLeftSize = left.map(_.size).max
    val leftPadded = left ++ Seq.fill(math.max(right.size - left.size, 0))("")
    val rightPadded = right ++ Seq.fill(math.max(left.size - right.size, 0))("")

    leftPadded.zip(rightPadded).map {
      case (l, r) => (if (l == r) " " else "!") + l + (" " * ((maxLeftSize - l.size) + 3)) + r
    }
  }

  def hasValuedVariable(formula: Formula, context: EvaluationContext): Boolean = {
    formula match {
      case v: Variable if (context.isDefined(v)) => true
      case func: BaseFunctionCall => func.parameters.exists { hasValuedVariable(_, context) }
      case _ => formula.children.exists { hasValuedVariable(_, context) }
    }
  }

}