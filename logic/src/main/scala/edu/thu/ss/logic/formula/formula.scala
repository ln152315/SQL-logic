package edu.thu.ss.logic.formula

import edu.thu.ss.logic.tree.TreeNode
import edu.thu.ss.logic.tree.NamedNode

abstract class Formula extends TreeNode[Formula] {

  def substitute(variable: Variable, value: Constant): Formula = {
    this.transform {
      case func: BaseFunctionCall => func.substitute(variable, value)
    }
  }

}

trait NamedFormula extends NamedNode {
  val name: Symbol
  var formula: Formula

}

object NamedFormula {
  implicit def toFormula(named: NamedFormula) = named.formula

}

abstract class BinaryFormula extends Formula {
  def left: Formula
  def right: Formula

  def children: Seq[Formula] = Seq(left, right)

  override def toString = s"($left $nodeName $right)"
}

abstract class UnaryFormula extends Formula {
  def child: Formula

  def children: Seq[Formula] = Seq(child)

  override def toString = s"$nodeName $child"
}

abstract class Term extends Formula {
  def children: Seq[Formula] = Nil

  override def argString = toString

}

