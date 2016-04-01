package edu.thu.ss.logic.formula

import java.lang.reflect.Method
import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser.AnalysisException
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.tree.NamedNode
import edu.thu.ss.logic.model.State

abstract class LogicDefinition extends NamedNode {
  val name: Symbol

}

trait ClassedDefinition[T] {
  def clazz: Class[_ <: T]
  def impl: T
}

case class Sort(name: Symbol, clazz: Class[_ <: ISort[_]]) extends LogicDefinition with ClassedDefinition[ISort[_]] {
  def nodeName = "sort"
  override val toString = name.toString

  //sort implementation is shared for all queries
  lazy val impl = clazz.getConstructor().newInstance()

}

object Sort {
  implicit def toSortImpl(sort: Sort) = sort.impl
}

case class Parameter(name: Symbol, sort: Sort) extends NamedNode {
  override def nodeName = "parameter"

  override def toString = s"$sort $name"
}

abstract class BaseFunctionDef[T <: IBaseFunction] extends LogicDefinition with ClassedDefinition[T] {
  def parameters: Seq[Parameter]
  def range: Sort
  def domain: Seq[Sort] = parameters.map(_.sort)

  //each query maintains a function implementation
  def impl: T = clazz.getConstructor().newInstance().asInstanceOf[T]

  lazy val evaluateMethod: Method =
    clazz.getMethod("evaluate", domain.map { _.valueClass }: _*)

  def evaluate(impl: IBaseFunction, state: State, params: Seq[Any]): Any = {
    //set current state
    impl.setState(state)

    val objs = params.map { _.asInstanceOf[AnyRef] }
    val value = evaluateMethod.invoke(impl, objs: _*)
    if (!range.isValidValue(value)) {
      throw new IllegalValueException(s"$value returned by function ${this} is not a valid value of ${range.nodeName} ${range.name}.")
    }
    value
  }

}

case class FunctionDef(name: Symbol, parameters: Seq[Parameter], range: Sort, clazz: Class[_ <: IFunction]) extends BaseFunctionDef[IFunction] with Equals {
  def nodeName = "function"

  override def toString = s"$range $name(${parameters.mkString(", ")})"

  def canEqual(other: Any) = {
    other.isInstanceOf[edu.thu.ss.logic.formula.FunctionDef]
  }

  override def equals(other: Any) = {
    other match {
      case that: edu.thu.ss.logic.formula.FunctionDef => that.canEqual(FunctionDef.this) && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + name.hashCode
  }

}

case class PredicateDef(name: Symbol, parameters: Seq[Parameter], clazz: Class[_ <: IPredicate]) extends BaseFunctionDef[IPredicate] with Equals {
  def nodeName = "predicate"
  val range: Sort = boolSort

  override def toString = s"$name(${parameters.mkString(", ")})"

  def canEqual(other: Any) = {
    other.isInstanceOf[edu.thu.ss.logic.formula.PredicateDef]
  }

  override def equals(other: Any) = {
    other match {
      case that: edu.thu.ss.logic.formula.PredicateDef => that.canEqual(PredicateDef.this) && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + name.hashCode
  }

}

case class FormulaDef(name: Symbol, var formula: Formula) extends LogicDefinition with NamedFormula with Equals {
  def nodeName = "formula"

  override def toString = s"$name=$formula"

  def canEqual(other: Any) = {
    other.isInstanceOf[edu.thu.ss.logic.formula.FormulaDef]
  }

  override def equals(other: Any) = {
    other match {
      case that: edu.thu.ss.logic.formula.FormulaDef => that.canEqual(FormulaDef.this) && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + name.hashCode
  }

}

class LogicDefinitions {
  private val sorts: mutable.Map[Symbol, Sort] = new mutable.HashMap

  private val functions: mutable.Map[Symbol, FunctionDef] = new mutable.HashMap

  private val predicates: mutable.Map[Symbol, PredicateDef] = new mutable.HashMap

  private val formulas: mutable.Map[Symbol, FormulaDef] = new mutable.HashMap

  def getSorts = sorts
  def getFunctions = functions
  def getPredicates = predicates
  def getFormulas = formulas

  //default
  addSort(boolSort)

  def addSort(sort: Sort) {
    if (sorts.contains(sort.name)) {
      throw new AnalysisException(sort.name)
    }

    sorts.put(sort.name, sort)
  }

  def addFunction(function: FunctionDef) {
    if (functions.contains(function.name)) {
      throw new AnalysisException(function.name)
    }

    functions.put(function.name, function)
  }

  def addPredicate(predicate: PredicateDef) {
    if (predicates.contains(predicate.name)) {
      throw new AnalysisException(predicate.name)
    }

    predicates.put(predicate.name, predicate)
  }

  def addFormula(formula: FormulaDef) {
    if (formulas.contains(formula.name)) {
      throw new AnalysisException(formula.name)
    }
    formulas.put(formula.name, formula)
  }

  def lookupSort(name: Symbol) = sorts.get(name)

  def lookupFunction(name: Symbol) = functions.get(name)

  def lookupPredicate(name: Symbol) = predicates.get(name)

  def lookupFormula(name: Symbol) = formulas.get(name)

  override def toString = s"""${sorts.values.mkString("\n")}
${functions.values.mkString("\n")}
${predicates.values.mkString("\n")}
${formulas.values.mkString("\n")}"""

}