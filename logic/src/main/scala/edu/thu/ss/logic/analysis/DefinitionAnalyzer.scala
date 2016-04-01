package edu.thu.ss.logic.analysis

import edu.thu.ss.logic.definition._
import edu.thu.ss.logic.formula._
import edu.thu.ss.logic.paser._
import edu.thu.ss.logic.util._

abstract class Analyzer[Type] extends Logging {
  protected var definitions: LogicDefinitions = null
  private var error = false

  protected def errorMsg: String

  def apply(list: Seq[Type], definitions: LogicDefinitions) {
    error = false
    this.definitions = definitions
    analyze(list)

    if (error) {
      throw AnalysisException(errorMsg)
    }
  }
  protected def analyze(list: Seq[Type])

  protected def setError(msg: => String): Null = {
    error = true
    logError(msg)
    null
  }
}

abstract class DefinitionAnalyzer extends Analyzer[UnresolvedDefinition] {

  protected val errorMsg = "#definition contains errors. See error messages above."
}

case class CheckDefinitionUnique() extends DefinitionAnalyzer {

  protected def analyze(udefs: Seq[UnresolvedDefinition]) {

    LogicUtils.checkUnique(udefs, _.asInstanceOf[UnresolvedDefinition].name, {
      case udef: UnresolvedDefinition =>
        setError(
          s"${udef.nodeName}'s name ${udef.name} has already been used somewhere else. Please choose another name.")
    })

    udefs.withFilter(_.isInstanceOf[UnresolvedBaseFunctionDef]).foreach {
      case ufunc: UnresolvedBaseFunctionDef =>
        LogicUtils.checkUnique(ufunc.parameters, _.asInstanceOf[UnresolvedParameter].name, {
          case uparam: UnresolvedParameter =>
            setError(
              s"Pamarater's name ${uparam.name} has already been used somewhere else in ${ufunc.nodeName} ${ufunc.name}. Please choose another name.")
        })
    }

  }
}

case class DefinitionResolver() extends DefinitionAnalyzer {

  def analyze(udefs: Seq[UnresolvedDefinition]) {
    //resolve sort first
    udefs.withFilter(_.isInstanceOf[UnresolvedSort]).foreach(usort => {
      val clazz = getClass(usort).asInstanceOf[Class[_ <: ISort[_]]]
      val sort = Sort(usort.name, clazz)
      definitions.addSort(sort)
    })

    udefs.withFilter(!_.isInstanceOf[UnresolvedSort]).foreach {
      case ufunc: UnresolvedFunctionDef => {
        val clazz = getClass(ufunc).asInstanceOf[Class[_ <: IFunction]]
        val params = resolveParameters(ufunc, ufunc.parameters)
        val range = definitions.lookupSort(ufunc.range)
        range match {
          case None =>
            setError(s"Undefined sort ${ufunc.range} for the range of ${ufunc.nodeName} ${ufunc.name}.")
          case Some(r) if (r == boolSort) =>
            //handle a special bool case
            setError(s"${ufunc.nodeName} ${ufunc} should not return bool values. Please make it as a predicate.")
          case Some(r) =>
            definitions.addFunction(FunctionDef(ufunc.name, params, r, clazz))
        }
      }

      case upred: UnresolvedPredicateDef => {
        val clazz = getClass(upred).asInstanceOf[Class[_ <: IPredicate]]
        val params = resolveParameters(upred, upred.parameters)
        definitions.addPredicate(PredicateDef(upred.name, params, clazz))
      }

      case udef: UnresolvedFormulaDef =>
        definitions.addFormula(FormulaDef(udef.name, udef.formula))
    }

    def getClass(udef: UnresolvedDefinition): Class[_] = {
      try {
        val clazz = Class.forName(udef.clazz)
        udef match {
          case sort: UnresolvedSort if (!classOf[ISort[_]].isAssignableFrom(clazz)) =>
            setError(s"Class ${udef.clazz} is not a subclass of ISort for ${udef.nodeName} ${udef.name}.")
          case func: UnresolvedFunctionDef if (!classOf[IFunction].isAssignableFrom(clazz)) =>
            setError(s"Class ${udef.clazz} is not a subclass of IFunction for ${udef.nodeName} ${udef.name}.")
          case pred: UnresolvedPredicateDef if (!classOf[IPredicate].isAssignableFrom(clazz)) =>
            setError(s"Class ${udef.clazz} is not a subclass of IPredicate for ${udef.nodeName} ${udef}.")
          case _ => clazz
        }

      } catch {
        case e: ClassNotFoundException =>
          setError(s"Class ${udef.clazz} is not found for ${udef.nodeName} ${udef.name}.")
      }
    }

  }

  protected def resolveParameters(udef: UnresolvedDefinition, uparams: Seq[UnresolvedParameter]): Seq[Parameter] = {
    uparams.map(uparam => {
      val sort = definitions.lookupSort(uparam.sort)
      sort match {
        case Some(s) => {
          Parameter(uparam.name, s)
        }
        case None => {
          setError(s"Undefined sort ${uparam.sort} for parameter ${uparam.name} in ${udef.nodeName} ${udef.name}.")
        }
      }
    })
  }
}

case class CheckClassCompatibility() extends DefinitionAnalyzer {
  protected def analyze(list: Seq[UnresolvedDefinition]) {
    definitions.getFunctions.values.foreach(checkFunction(_))
    definitions.getPredicates.values.foreach(checkFunction(_))
  }

  protected def checkFunction(func: BaseFunctionDef[_ <: IBaseFunction]) {
    try {
      val method = func.evaluateMethod
      //check method range
      val returnType = method.getReturnType
      val expectedType = func.range.valueClass
      if (!expectedType.isAssignableFrom(returnType)) {
        setError(s"Method ${getEvaluateString(func)} returns wrong type (expected: ${expectedType}, provided: ${returnType}) in ${func.nodeName} ${func.toString}.")
      }
    } catch {
      case e: NoSuchMethodException =>
        setError(s"Method ${getEvaluateString(func)} is not found in ${func.clazz} for ${func.nodeName} ${func.toString}.")
    }
  }

  protected def getEvaluateString(func: BaseFunctionDef[_ <: IBaseFunction]): String = {
    s"evaluate(${func.domain.map(_.valueClass).mkString(", ")})"
  }
}

