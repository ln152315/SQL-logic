package edu.thu.ss.logic.paser

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.tree.NamedNode

trait UnresolvedElement {
  def name: Symbol

}

case class UnresolvedFunctionCall(name: Symbol, parameters: Seq[Term]) extends Term with UnresolvedElement {
  val nodeName = name.toString

  override def toString = s"$nodeName(${parameters.mkString(", ")})"
}

class UnresolvedVariable(override val name: Symbol, val usort: Symbol) extends Variable(name, null) with UnresolvedElement {

  override def toString = name.toString
}

abstract class UnresolvedDefinition extends NamedNode with UnresolvedElement {
  def clazz: String

}

case class UnresolvedSort(name: Symbol, clazz: String) extends UnresolvedDefinition {
  val nodeName = "sort"

  override def toString = name

}

case class UnresolvedParameter(name: Symbol, sort: Symbol) extends UnresolvedElement {
  val kind = "parameter"

  override def toString = s"$sort $name"
}

abstract class UnresolvedBaseFunctionDef extends UnresolvedDefinition {
  def parameters: Seq[UnresolvedParameter]
  def range: Symbol

  override def toString = s"$range $name(${parameters.mkString(", ")})"

}

case class UnresolvedFunctionDef(name: Symbol, parameters: Seq[UnresolvedParameter], range: Symbol, clazz: String) extends UnresolvedBaseFunctionDef {
  override def toString = s"$range $name(${parameters.mkString(", ")})"

  val nodeName = "function"
}

case class UnresolvedPredicateDef(name: Symbol, parameters: Seq[UnresolvedParameter], clazz: String) extends UnresolvedBaseFunctionDef {
  val range: Symbol = "bool"

  override def toString = s"bool $name(${parameters.mkString(", ")})"

  val nodeName = "predicate"

}

case class UnresolvedFormulaDef(name: Symbol, formula: Formula) extends UnresolvedDefinition {
  val clazz = ""

  val nodeName = "formula"
}

case class UnresolvedConstant(value: String) extends Term {

  val nodeName = "constant"
}
