package edu.thu.ss.logic.paser

import edu.thu.ss.logic.tree.TreeNode

case class ParseException(msg: String) extends Exception(msg) {
}

case class AnalysisException(msg: String) extends Exception(msg) {
}

class TreeNodeException[Type <: TreeNode[_]](
  tree: Type, msg: String, cause: Throwable)
    extends Exception(msg, cause) {

  def this(tree: Type, msg: String) = this(tree, msg, null)

  override def getMessage: String = {
    val treeString = tree.toString
    s"${super.getMessage}, tree:${if (treeString contains "\n") "\n" else " "}$tree"
  }
}

case class IllegalValueException(msg: String) extends Exception(msg) {

}