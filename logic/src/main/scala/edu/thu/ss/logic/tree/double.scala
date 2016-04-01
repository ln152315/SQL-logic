package edu.thu.ss.logic.tree

//double linked node
abstract class DoubleNode[BaseType <: DoubleNode[BaseType]] extends TreeNode[BaseType] {
  self: BaseType =>

  def parent: BaseType

  def findParent(f: BaseType => Boolean): Option[BaseType] = f(this) match {
    case true => Some(this)
    case false =>
      if (parent != null) {
        parent.findParent(f)
      } else {
        None
      }
  }

  def foreachParent(f: BaseType => Unit): Unit = {
    f(this)
    if (parent != null) {
      parent.foreachParent(f)
    } else {
      None
    }
  }

  def forallParent(p: BaseType => Boolean): Boolean = {
    val result = p(this)
    if (!result) {
      return false
    }
    if (parent != null) {
      parent.forallParent(p)
    } else {
      true
    }
  }

  def existsParent(p: BaseType => Boolean): Boolean = {
    val result = p(this)
    if (result) {
      return true
    }
    if (parent != null) {
      parent.existsParent(p)
    } else {
      false
    }
  }
  def mapParent[A](f: BaseType => A): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreachParent(ret += f(_))
    ret
  }

  def flatMapParent[A](f: BaseType => TraversableOnce[A]): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreachParent(ret ++= f(_))
    ret
  }

  def collectParent[B](pf: PartialFunction[BaseType, B]): Seq[B] = {
    val ret = new collection.mutable.ArrayBuffer[B]()
    val lifted = pf.lift
    foreachParent(node => lifted(node).foreach(ret.+=))
    ret
  }

  def collectFirstParent[B](pf: PartialFunction[BaseType, B]): Option[B] = {
    val lifted = pf.lift
    lifted(this).orElse {
      if (parent != null) {
        parent.collectFirstParent(pf)
      } else {
        None
      }
    }
  }

  override def mapChildren(f: BaseType => BaseType): BaseType = throw new UnsupportedOperationException

  override def transformDown(rule: PartialFunction[BaseType, BaseType], postRule: PartialFunction[BaseType, Unit] = PartialFunction.empty): BaseType = throw new UnsupportedOperationException
  override def transformUp(rule: PartialFunction[BaseType, BaseType]): BaseType = throw new UnsupportedOperationException

}