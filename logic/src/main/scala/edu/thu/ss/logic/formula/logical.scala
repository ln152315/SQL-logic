package edu.thu.ss.logic.formula

import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.util.LogicUtils
import edu.thu.ss.logic.definition.IBaseFunction

case class Not(child: Formula) extends UnaryFormula {
  def nodeName = "NOT"

}

case class And(left: Formula, right: Formula) extends BinaryFormula {
  def nodeName = "AND"
  
}

case class Or(left: Formula, right: Formula) extends BinaryFormula {
  def nodeName = "OR"

}

case class Imply(left: Formula, right: Formula) extends BinaryFormula {
  def nodeName = "IMPLY"
  
  

}

abstract class Quantifier extends UnaryFormula {
  def variable: Variable

  var quantifiedPredicate: PredicateCall

  override def toString: String = {
    s"$nodeName ${variable.sort} ${variable.name}. ($child)"
  }

  override def simpleString = s"$nodeName ${variable.sort} ${variable.name}."

}

case class Forall(variable: Variable, child: Formula, var quantifiedPredicate: PredicateCall = null) extends Quantifier {
  def nodeName = "forall"

}

case class Exists(variable: Variable, child: Formula, var quantifiedPredicate: PredicateCall = null) extends Quantifier {
  def nodeName = "exists"

}

case class Symbol(value: String) extends Term {
  def nodeName = "symbol"

  override def toString: String = value
}

object Symbol {
  implicit def toSymbol(name: String): Symbol = Symbol(name)

  implicit def toString(symbol: Symbol): String = symbol.value

}

case class Variable(name: Symbol, sort: Sort) extends Term {
  def nodeName = "variable"

  override def toString = s"$name"

}
case class Constant(value: Any) extends Term {
  def nodeName = "constant"

  override def toString = {
    if (LogicUtils.isNumericalValue(value)) {
      value.toString
    } else {
      '"' + value.toString + '"'
    }
  }

}

abstract class BaseFunctionCall extends Term {
  def definition: BaseFunctionDef[_ <: IBaseFunction]
  def parameters: Seq[Term]

  override def toString = s"${definition.name}(${parameters.mkString(", ")})"

  protected def substituteParams(variable: Variable, value: Constant): Seq[Term] = {
    if (!parameters.contains(variable)) {
      parameters
    } else {
      parameters.map {
        case v: Variable if (v == variable) => value
        case func: BaseFunctionCall => func.substitute(variable, value).asInstanceOf[BaseFunctionCall]
        case term: Term => term
      }
    }
  }
}

case class FunctionCall(definition: FunctionDef, parameters: Seq[Term]) extends BaseFunctionCall {
  def nodeName = "function"
  override def substitute(variable: Variable, value: Constant): FunctionCall = {
    val substitutedParams = substituteParams(variable, value)
    if (substitutedParams.eq(parameters)) {
      this
    } else {
      FunctionCall(definition, substituteParams(variable, value))
    }
  }
}

case class PredicateCall(definition: PredicateDef, parameters: Seq[Term]) extends BaseFunctionCall {

  def nodeName = "predicate"
  override def substitute(variable: Variable, value: Constant): PredicateCall = {
    val substitutedParams = substituteParams(variable, value)
    if (substitutedParams.eq(parameters)) {
      this
    } else {
      PredicateCall(definition, substituteParams(variable, value))
    }
  }

}

