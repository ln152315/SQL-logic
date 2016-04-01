package edu.thu.ss.logic.policy

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.formula.LogicDefinitions
import scala.collection.mutable
import edu.thu.ss.logic.tree.TreeNode

case class Rule(name: Symbol, var formula: Formula) extends TreeNode[Rule] with NamedFormula {

  val children = Nil

  val nodeName = "rule"

  override def toString = s"$name: $formula"

}

class Policy(val definitions: LogicDefinitions, val rules: Seq[Rule]) {

  override def toString = s"""#defition
${definitions.toString}

#policy
${rules.mkString("\n")}"""

}