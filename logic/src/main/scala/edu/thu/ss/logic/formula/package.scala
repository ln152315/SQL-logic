
package edu.thu.ss.logic

import edu.thu.ss.logic.formula.True
import edu.thu.ss.logic.formula.False
import edu.thu.ss.logic.definition.ISort

class BoolSortImpl extends ISort[Boolean] {

  val valueClass = classOf[Boolean]

  def validInput(input: String): Boolean = {
    input == "true" || input == "false"
  }

  def parseInput(input: String): Boolean = {
    input == True.toString()
  }

  protected def validValue(value: Boolean) = true

  override val finite = true

  override val values = Set(true, false)

}

/**
 * Predefined logic elements
 */

package object formula {

  val boolSort = new Sort("bool", classOf[BoolSortImpl])

  val True = Constant(true)

  val False = Constant(false)
}
