package edu.thu.ss.logic.evaluate

import scala.collection.mutable
import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import sun.org.mozilla.javascript.internal.BaseFunction
import edu.thu.ss.logic.paser.IllegalValueException
import edu.thu.ss.logic.model.QueryModel
import edu.thu.ss.logic.model.State
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.util.LogicUtils
/**
 * a context for each model
 */
class EvaluationContext(val model: QueryModel) {

  private val functionImpls = new mutable.HashMap[Symbol, IBaseFunction]

  //only used when fineCache is disabled
  private val valuation = new mutable.HashMap[Symbol, Any]

  def clear {
    functionImpls.clear
    valuation.clear
  }

  def getFunctionImpl(function: FunctionDef): IFunction = {
    getBaseFunctionImpl(function).asInstanceOf[IFunction]
  }

  def getPredicateImpl(predicate: PredicateDef): IPredicate = {
    getBaseFunctionImpl(predicate).asInstanceOf[IPredicate]
  }

  def getBaseFunctionImpl(base: BaseFunctionDef[_ <: IBaseFunction]): IBaseFunction = {
    functionImpls.getOrElseUpdate(base.name, base.impl).asInstanceOf[IBaseFunction]
  }

  def getValue(variable: Variable) = valuation.get(variable.name).get

  def setValue(variable: Variable, value: Any) {
    valuation.put(variable.name, value)
  }

  def isDefined(variable: Variable) = valuation.contains(variable.name)

  def removeValue(variable: Variable) {
    valuation.remove(variable.name)
  }

}

class FormulaEvaluator(val model: QueryModel, val fineCache: Boolean) extends Logging {

  private lazy val context = new EvaluationContext(model)

  def evaluate(formula: Formula): Boolean = {
    val value = model.initialStates.forall(evaluateFormula(formula, _))

    //the context and cache are shared among all evaluations of a formula on a model (multiple initial states)
    context.clear
    model.clearCache

    value
  }

  private def evaluateFormula(formula: Formula, state: State): Boolean = {
    val cacheEnabled = fineCache || !LogicUtils.hasValuedVariable(formula, context)

    if (cacheEnabled) {
      val cacheResult = state.getFormula(formula)
      if (cacheResult.isDefined) {
        logTrace(s"cache hit for formula $formula")
        return cacheResult.get
      }
    }

    val result =
      formula match {
        case Not(child) =>
          !(evaluateFormula(child, state))

        case And(left, right) =>
          evaluateFormula(left, state) && evaluateFormula(right, state)

        case Or(left, right) =>
          evaluateFormula(left, state) || evaluateFormula(right, state)

        case Imply(left, right) =>
          !evaluateFormula(left, state) || evaluateFormula(right, state)

        case pred: PredicateCall =>
          evaluateFunction(pred, state).asInstanceOf[Boolean]

        case quantifier: Quantifier =>
          evaluateQuantifier(quantifier, state)

        case variable: Variable =>
          //must be a boolean variable
          context.getValue(variable).asInstanceOf[Boolean]

        case True => true

        case False => false

        //temporal part
        case _: AG | _: EG =>
          val g = formula.asInstanceOf[UnaryFormula]
          evaluateFormula(g.child, state) &&
            (state.parent == null || evaluateFormula(g, state.parent))

        case _: AF | _: EF =>
          //child holds in one of the current plus all parent states
          val f = formula.asInstanceOf[UnaryFormula]
          evaluateFormula(f.child, state) ||
            (state.parent != null && evaluateFormula(f, state.parent))

        case _: AU | _: EU =>
          //either right holds in the current state, or left holds in the current state and u holds in the parent state
          val u = formula.asInstanceOf[BinaryFormula with TemporalFormula]
          evaluateFormula(u.right, state) ||
            (state.parent != null && evaluateFormula(u.left, state) && evaluateFormula(u, state.parent))

        case ax: AX =>
          //either the parent state does not exist, or child holds in the parent state        
          state.parent == null || evaluateFormula(ax.child, state.parent)
        case ex: EX =>
          //the parent state must exist, and child holds in the parent state
          state.parent != null && evaluateFormula(ex.child, state.parent)

        case pag: pAG =>
          //child holds in the current plus all children states
          evaluateFormula(pag.child, state) &&
            state.children.forall { evaluateFormula(pag, _) }
        case peg: pEG =>
          //child holds in the current state or one of the children states (if any) 
          evaluateFormula(peg.child, state) &&
            (state.children.isEmpty || state.children.exists { evaluateFormula(peg, _) })

        case paf: pAF =>
          //either child holds in the current state, or paf holds in all children states (non-empty)  
          evaluateFormula(paf.child, state) ||
            (!state.children.isEmpty && state.children.forall { evaluateFormula(paf, _) })
        case pef: pEF =>
          //child holds in one of the current plus all children states
          evaluateFormula(pef.child, state) ||
            state.children.exists { evaluateFormula(pef, _) }

        case pas: pAS =>
          //either right holds in the current state, or left holds in the current state and pas holds in all children states (non-empty)
          evaluateFormula(pas.right, state) ||
            (!state.children.isEmpty && evaluateFormula(pas.left, state)
              && state.children.forall { evaluateFormula(pas, _) })
        case pes: pES =>
          //either right holds in the current state, or left holds in the current state and pau holds in one of children states (non-empty)
          evaluateFormula(pes.right, state) ||
            (!state.children.isEmpty && evaluateFormula(pes.left, state)
              && state.children.exists { evaluateFormula(pes, _) })

        case pax: pAX =>
          //child holds in all children states (if any)
          state.children.forall { evaluateFormula(pax.child, _) }
        case pex: pEX =>
          //child holds in one of the children states (at least one, which means non-empty)
          state.children.exists { evaluateFormula(pex.child, _) }

      }
    if (cacheEnabled) {
      logTrace(s"cache formula $formula")
      state.cacheFormula(formula, result)
    }
    result
  }

  private def evaluateFunction(function: BaseFunctionCall, state: State): Any = {
    val functionDef = function.definition
    val impl = context.getBaseFunctionImpl(functionDef)
    val params = function.parameters.map(evaluateParam(_, state))
    functionDef.evaluate(impl, state, params)
  }

  private def evaluateParam(param: Term, state: State): Any = {
    param match {
      case subfunc: BaseFunctionCall => evaluateFunction(subfunc, state)
      case const: Constant => const.value
      case variable: Variable =>
        // must have been initialized
        context.getValue(variable)
    }
  }

  private def evaluateQuantifier(quantifier: Quantifier, state: State): Boolean = {
    val variable = quantifier.variable
    val quantifiedPredicate = quantifier.quantifiedPredicate

    if (quantifier.child.isInstanceOf[PredicateCall] && quantifiedPredicate != null) {
      //handle single quantified predicate
      val result = quantifier match {
        case forall: Forall =>
          if (!variable.sort.finite) {
            false
          } else {
            val quantifiedValues = getQuantifiedValues(quantifiedPredicate, variable, state).toSet
            variable.sort.values.forall { quantifiedValues.contains(_) }
          }
        case exists: Exists =>
          !getQuantifiedValues(quantifiedPredicate, variable, state).isEmpty
      }
      return result
    }

    val transformed =
      if (quantifiedPredicate != null && !quantifier.child.isInstanceOf[PredicateCall]) {
        quantifier.child.transform {
          //ignore the evaluation of the quantified predicate  
          case pred: PredicateCall if (pred == quantifiedPredicate) => True
        }
      } else {
        quantifier.child
      }

    val quantifiedValues = getQuantifiedValues(quantifier, state)

    if (fineCache) {
      //in this case, we would substitute the formula directly
      quantifier match {
        case forall: Forall => quantifiedValues.forall(value => {
          val substituted = transformed.substitute(variable, Constant(value))
          evaluateFormula(substituted, state)
        })
        case exists: Exists => quantifiedValues.exists(value => {
          val substituted = transformed.substitute(variable, Constant(value))
          evaluateFormula(substituted, state)
        })
      }
    } else {
      //otherwise, formula with free variables is not cached, and we store variable values into context
      val result = quantifier match {
        case forall: Forall => quantifiedValues.forall(value => {
          context.setValue(variable, value)
          evaluateFormula(transformed, state)
        })
        case exists: Exists => quantifiedValues.exists(value => {
          context.setValue(variable, value)
          evaluateFormula(transformed, state)
        })
      }
      context.removeValue(quantifier.variable)
      result
    }

  }

  private def getQuantifiedValues(quantifier: Quantifier, state: State): Traversable[Any] = {
    if (quantifier.quantifiedPredicate != null) {
      getQuantifiedValues(quantifier.quantifiedPredicate, quantifier.variable, state)
    } else {
      quantifier.variable.sort.values
    }
  }

  private def getQuantifiedValues(predicate: PredicateCall, variable: Variable, state: State): Traversable[Any] = {
    val impl = context.getPredicateImpl(predicate.definition)

    val index = predicate.parameters.indexOf(variable)
    val otherParams = predicate.parameters.withFilter(_ != variable).map(evaluateParam(_, state))
    val values = impl.values(index, otherParams)
    //TODO: should we check values?
    values.foreach(value => {
      if (!variable.sort.isValidValue(value)) {
        throw new IllegalValueException(s"$value returned by ${predicate.definition.clazz} is not a valid value of ${variable.sort.nodeName} ${variable.sort.name}.")
      }
    })
    values
  }

}