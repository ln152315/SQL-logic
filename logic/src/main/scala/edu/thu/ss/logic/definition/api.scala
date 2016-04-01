package edu.thu.ss.logic.definition

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False
import java.{ lang => java }
import edu.thu.ss.logic.model.State

trait IDefinition {

}

trait ISort[T] extends IDefinition {

  val valueClass: Class[T]

  def validInput(input: String): Boolean

  def parseInput(input: String): T

  def isValidValue(value: Any): Boolean = {
    try {
      validValue(value.asInstanceOf[T])
    } catch {
      case t: ClassCastException =>
        t.printStackTrace()
        false
    }
  }

  def values: Traversable[T] = throw new UnsupportedOperationException

  def finite: Boolean = false

  protected def validValue(value: T): Boolean

}

trait IBaseFunction {
  private var state: State = null

  def setState(state: State) {
    this.state = state
  }

  def getState = state

}

trait IFunction extends IBaseFunction {

}

trait IPredicate extends IBaseFunction {

  def finite(index: Int): Boolean = false

  def values(index: Int, otherParams: Seq[Any]): Seq[Any] = throw new UnsupportedOperationException

}

