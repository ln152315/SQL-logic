package edu.thu.ss.logic.analysis

import scala.collection.mutable
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser.{ UnresolvedFunctionCall, UnresolvedVariable }
import edu.thu.ss.logic.util.LogicUtils
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.UnresolvedConstant
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.UnresolvedFunctionCall
import edu.thu.ss.logic.paser.UnresolvedConstant
import edu.thu.ss.logic.definition.IBaseFunction
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

abstract class FormulaAnalyzer extends Analyzer[NamedFormula] {

  protected val errorMsg = "#policy contains errors. See error messages above."

}

abstract class SequentialAnalyzer extends FormulaAnalyzer {
  protected var curFormula: NamedFormula = null
  
  protected def analyze(formulas: Seq[NamedFormula]) {
    formulas.foreach(formula => {
      curFormula = formula
      analyzeFormula()
    })
  }

  protected def analyzeFormula()
}

case class FormulaExpander() extends SequentialAnalyzer {

  private val expanded = new mutable.HashSet[Symbol]

  override protected def analyzeFormula() {
    curFormula.formula = expand(curFormula, new mutable.HashSet[Symbol])
    expanded.add(curFormula.name)
  }

  private def expand(formula: Formula, reached: mutable.HashSet[Symbol]): Formula = {
    formula.transform({
      case symbol: Symbol => {
        definitions.lookupFormula(symbol) match {
          case Some(f) if (reached.contains(f.name)) => {
            setError(s"${curFormula.nodeName} ${curFormula.name} contains cycle formula reference. Please fix.")
          }
          case Some(f) =>
            reached.add(f.name)
            if (!expanded.contains(f.name)) {
              f.formula = expand(f.formula, reached)
              expanded.add(f.name)
            }
            f.formula
          case None => symbol
        }
      }
    })
  }
}

case class CheckFormulaUnique() extends FormulaAnalyzer {
  protected def analyze(formulas: Seq[NamedFormula]) {
    LogicUtils.checkUnique(formulas, _.asInstanceOf[NamedFormula].name, {
      case formula: NamedFormula =>
        setError(
          s"${formula.nodeName}'s name ${formula.name} has already been used somewhere else. Please choose another name.")
    })

    formulas.foreach(formula => checkBoundVariables(formula, formula.name))

    formulas.foreach(formula => {
      val variables = formula.map({
        case quantifier: Quantifier => quantifier.variable
        case _ => null
      }).filter { _ != null }
      LogicUtils.checkUnique(variables, _.asInstanceOf[Variable].name, {
        case v: Variable => {
        }

      })
    })
  }

  private def checkBoundVariables(formula: Formula, name: String, variables: mutable.Set[Symbol] = new HashSet) {
    formula match {
      case quantifier: Quantifier =>
        if (variables.contains(quantifier.variable.name)) {
          setError(
            s"${quantifier.variable.nodeName}'s name ${quantifier.variable.name} has already been used in formula ${name}. Please choose another name.")
        }
        variables.add(quantifier.variable.name)
        checkBoundVariables(quantifier.child, name, variables)
        variables.remove(quantifier.variable.name)

      case _ => formula.children.foreach { checkBoundVariables(_, name, variables) }
    }

  }

}

case class FormulaResolver() extends SequentialAnalyzer {

  private val context = new mutable.HashMap[Symbol, Variable]

  def analyzeFormula() {
    context.clear
    curFormula.formula = resolve(curFormula)
  }

  private def resolve(formula: Formula): Formula = {
    formula.transformDown({
      case quantifier: Quantifier => {
        resolveQuantifier(quantifier)
      }
      case ufunc: UnresolvedFunctionCall => {
        val call = resolveFunctionCall(ufunc)
        call match {
          case func: FunctionCall => {
            //only predicate is allowed
            setError(s"${func.nodeName} ${func} is not a predicate.")
          }
          case _ => call
        }
      }
      case symbol: Symbol => {
        val variable = context.get(symbol)
        variable match {
          case Some(v) if v.sort != boolSort => {
            setError(s"${v.sort} ${v.nodeName} ${v.name} is not a ${boolSort.name} ${v.nodeName}")
          }
          case Some(v) => v
          case None =>
            setError(s"Undefined variable ${symbol} in ${curFormula.nodeName} ${curFormula.name}")
        }
      }
    }, {
      //remove variables from context
      case quantifier: Quantifier => context.remove(quantifier.variable.name)
    })

  }

  private def resolveQuantifier(quantifier: Quantifier): Quantifier = {
    val uvar = quantifier.variable.asInstanceOf[UnresolvedVariable]
    val variable = definitions.lookupSort(uvar.usort) match {
      case Some(sort) => Variable(uvar.name, sort)
      case None =>
        setError(s"Undefined sort ${uvar.usort} for ${uvar.nodeName} ${uvar.name} in ${curFormula.nodeName} ${curFormula.name}")
        Variable(uvar.name, null)
    }

    context.put(variable.name, variable)
    quantifier match {
      case forall: Forall => Forall(variable, forall.child)
      case exist: Exists => Exists(variable, exist.child)
    }
  }

  private def resolveFunctionCall(ufunc: UnresolvedFunctionCall): BaseFunctionCall = {

    def resolveFunctionCall(funcDef: BaseFunctionDef[_ <: IBaseFunction], call: Seq[Term] => BaseFunctionCall): BaseFunctionCall = {
      if (funcDef.parameters.length != ufunc.parameters.length) {
        setError(s"Incorrect number of parameters for ${ufunc.nodeName} ${funcDef} in ${curFormula.nodeName} ${curFormula.name}")
      } else {
        val paramDef = funcDef.parameters.iterator
        val params = ufunc.parameters.map { param => resolveParameter(param, paramDef.next, funcDef) }
        call(params)
      }
    }

    if (definitions.lookupFunction(ufunc.name).isDefined) {
      val functionDef = definitions.lookupFunction(ufunc.name).get
      resolveFunctionCall(functionDef, FunctionCall(functionDef, _))
    } else if (definitions.lookupPredicate(ufunc.name).isDefined) {
      val predicateDef = definitions.lookupPredicate(ufunc.name).get
      resolveFunctionCall(predicateDef, PredicateCall(predicateDef, _))
    } else {
      setError(s"Undefined function ${ufunc.name} in ${curFormula.nodeName} ${curFormula.name}")
    }
  }

  private def resolveParameter(param: Term, paramDef: Parameter, funcDef: BaseFunctionDef[_ <: IBaseFunction]): Term = {
    def checkSort(provided: Sort) {
      if (provided != paramDef.sort) {
        setError(s"Incompatible argument for ${paramDef.nodeName} ${paramDef.name} (expected: ${paramDef.sort.name}, provided: ${provided.name}) in ${funcDef.nodeName} ${funcDef} for ${curFormula.nodeName} ${curFormula.name}")
      }
    }

    param match {
      case ufunc: UnresolvedFunctionCall =>
        val func = resolveFunctionCall(ufunc)
        checkSort(func.definition.range)
        func

      case uvar: Symbol =>
        context.get(uvar) match {
          case Some(v) =>
            checkSort(v.sort)
            v
          case None =>
            setError(s"Undefined variable ${uvar} in ${curFormula.nodeName} ${curFormula.name}")
        }
      case True | False =>
        checkSort(boolSort)
        param
      case const: UnresolvedConstant =>
        if (!paramDef.sort.validInput(const.value)) {
          setError(s"${const.value} is not a valid value for ${paramDef.nodeName} ${paramDef.name} in ${funcDef.nodeName} ${funcDef} of ${curFormula.nodeName} ${curFormula.name}.")
        } else {
          Constant(paramDef.sort.parseInput(const.value))
        }
    }
  }
}

case class CheckDecidability() extends SequentialAnalyzer {
  protected def analyzeFormula() {
    curFormula.foreach {
      case quantifier: Quantifier => {
        var decidable = false
        val variable = quantifier.variable
        if (variable.sort.finite) {
          decidable = true
        }

        val candidate = quantifier.child match {
          case pred: PredicateCall => pred
          case imply: Imply if (quantifier.isInstanceOf[Forall]) => imply.left
          case and: And if (quantifier.isInstanceOf[Exists]) => and
        }

        val pred = getQuantifiedPredicate(candidate, variable)
        if (pred.isDefined) {
          quantifier.quantifiedPredicate = pred.get
          decidable = true
        }

        if (!decidable) {
          setError(
            s"${curFormula.nodeName} ${curFormula.name}: ${curFormula.formula} is undecidable since ${variable.nodeName} ${variable.name} has inifite sort. Please add a proper quantified predicate for ${variable.nodeName} ${variable.name}.")
        }
      }
      case _ =>
    }
  }

  protected def getQuantifiedPredicate(formula: Formula, variable: Variable): Option[PredicateCall] = {
    formula match {
      case And(left, right) =>
        val leftPred = getQuantifiedPredicate(left, variable)
        if (leftPred.isDefined) {
          leftPred
        } else {
          getQuantifiedPredicate(right, variable)
        }
      case pred: PredicateCall if (checkQuantifiedPredicate(pred, variable)) =>
        Some(pred)
      case _ => None
    }
  }

  protected def checkQuantifiedPredicate(predicate: PredicateCall, variable: Variable): Boolean = {
    val index = predicate.parameters.indexOf(variable)
    index >= 0 && predicate.definition.impl.finite(index)
  }

}

