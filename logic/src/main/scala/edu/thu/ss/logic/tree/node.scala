package edu.thu.ss.logic.tree

import edu.thu.ss.logic.paser.TreeNodeException

trait NamedNode {
  def nodeName: String

}

abstract class TreeNode[BaseType <: TreeNode[BaseType]] extends NamedNode with Product {
  self: BaseType =>

  def children: Seq[BaseType]

  def containsChild: Set[TreeNode[_]] = children.toSet

  def fastEquals(other: TreeNode[_]): Boolean = {
    this.eq(other) || this == other
  }

  protected def otherCopyArgs: Seq[AnyRef] = Nil

  /**
   * Find the first [[TreeNode]] that satisfies the condition specified by `f`.
   * The condition is recursively applied to this node and all of its children (pre-order).
   */
  def find(f: BaseType => Boolean): Option[BaseType] = f(this) match {
    case true => Some(this)
    case false => children.foldLeft(None: Option[BaseType]) { (l, r) => l.orElse(r.find(f)) }
  }

  /**
   * Runs the given function on this node and then recursively on [[children]].
   * @param f the function to be applied to each node in the tree.
   */
  def foreach(f: BaseType => Unit): Unit = {
    f(this)
    children.foreach(_.foreach(f))
  }

  /**
   * Runs the given function recursively on [[children]] then on this node.
   * @param f the function to be applied to each node in the tree.
   */
  def foreachUp(f: BaseType => Unit): Unit = {
    children.foreach(_.foreachUp(f))
    f(this)
  }

  /**
   * Returns a Seq containing the result of applying the given function to each
   * node in this tree in a preorder traversal.
   * @param f the function to be applied.
   */
  def map[A](f: BaseType => A): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreach(ret += f(_))
    ret
  }

  /**
   * Returns a Seq by applying a function to all nodes in this tree and using the elements of the
   * resulting collections.
   */
  def flatMap[A](f: BaseType => TraversableOnce[A]): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreach(ret ++= f(_))
    ret
  }

  def forall(p: BaseType => Boolean): Boolean = {
    p(this) && children.forall { _.forall(p) }
  }

  def exists(p: BaseType => Boolean): Boolean = {
    p(this) || children.exists { _.exists { p } }
  }

  /**
   * Returns a Seq containing the result of applying a partial function to all elements in this
   * tree on which the function is defined.
   */
  def collect[B](pf: PartialFunction[BaseType, B]): Seq[B] = {
    val ret = new collection.mutable.ArrayBuffer[B]()
    val lifted = pf.lift
    foreach(node => lifted(node).foreach(ret.+=))
    ret
  }

  /**
   * Finds and returns the first [[TreeNode]] of the tree for which the given partial function
   * is defined (pre-order), and applies the partial function to it.
   */
  def collectFirst[B](pf: PartialFunction[BaseType, B]): Option[B] = {
    val lifted = pf.lift
    lifted(this).orElse {
      children.foldLeft(None: Option[B]) { (l, r) => l.orElse(r.collectFirst(pf)) }
    }
  }

  /**
   * Returns a copy of this node where `f` has been applied to all the nodes children.
   */
  def mapChildren(f: BaseType => BaseType): BaseType = {
    var changed = false
    val newArgs = productIterator.map {
      case arg: TreeNode[_] if containsChild(arg) =>
        val newChild = f(arg.asInstanceOf[BaseType])
        if (newChild fastEquals arg) {
          arg
        } else {
          changed = true
          newChild
        }
      case nonChild: AnyRef => nonChild
      case null => null
    }.toArray
    if (changed) makeCopy(newArgs) else this
  }

  /**
   * Returns a copy of this node with the children replaced.
   * TODO: Validate somewhere (in debug mode?) that children are ordered correctly.
   */
  def withNewChildren(newChildren: Seq[BaseType]): BaseType = {
    assert(newChildren.size == children.size, "Incorrect number of children")
    var changed = false
    val remainingNewChildren = newChildren.toBuffer
    val remainingOldChildren = children.toBuffer
    val newArgs = productIterator.map {
      // Handle Seq[TreeNode] in TreeNode parameters.
      case s: Seq[_] => s.map {
        case arg: TreeNode[_] if containsChild(arg) =>
          val newChild = remainingNewChildren.remove(0)
          val oldChild = remainingOldChildren.remove(0)
          if (newChild fastEquals oldChild) {
            oldChild
          } else {
            changed = true
            newChild
          }
        case nonChild: AnyRef => nonChild
        case null => null
      }
      case arg: TreeNode[_] if containsChild(arg) =>
        val newChild = remainingNewChildren.remove(0)
        val oldChild = remainingOldChildren.remove(0)
        if (newChild fastEquals oldChild) {
          oldChild
        } else {
          changed = true
          newChild
        }
      case nonChild: AnyRef => nonChild
      case null => null
    }.toArray

    if (changed) makeCopy(newArgs) else this
  }

  def transform(rule: PartialFunction[BaseType, BaseType], postRule: PartialFunction[BaseType, Unit] = PartialFunction.empty): BaseType = {
    transformDown(rule, postRule)
  }

  def transformDown(rule: PartialFunction[BaseType, BaseType], postRule: PartialFunction[BaseType, Unit] = PartialFunction.empty): BaseType = {
    val afterRule = rule.applyOrElse(this, identity[BaseType])

    // Check if unchanged and then possibly return old copy to avoid gc churn.
    val result = if (this fastEquals afterRule) {
      transformChildren(rule, (t, r) => t.transformDown(r, postRule))
    } else {
      afterRule.transformChildren(rule, (t, r) => t.transformDown(r, postRule))
    }

    if (postRule.isDefinedAt(result)) {
      postRule.apply(result)
    }

    result
  }

  /**
   * Returns a copy of this node where `rule` has been recursively applied to all the children of
   * this node.  When `rule` does not apply to a given node it is left unchanged.
   * @param rule the function used to transform this nodes children
   */
  protected def transformChildren(
    rule: PartialFunction[BaseType, BaseType],
    nextOperation: (BaseType, PartialFunction[BaseType, BaseType]) => BaseType): BaseType = {
    var changed = false
    val newArgs = productIterator.map {
      case arg: TreeNode[_] if containsChild(arg) =>
        val newChild = nextOperation(arg.asInstanceOf[BaseType], rule)
        if (!(newChild fastEquals arg)) {
          changed = true
          newChild
        } else {
          arg
        }
      case Some(arg: TreeNode[_]) if containsChild(arg) =>
        val newChild = nextOperation(arg.asInstanceOf[BaseType], rule)
        if (!(newChild fastEquals arg)) {
          changed = true
          Some(newChild)
        } else {
          Some(arg)
        }
      case args: Traversable[_] => args.map {
        case arg: TreeNode[_] if containsChild(arg) =>
          val newChild = nextOperation(arg.asInstanceOf[BaseType], rule)
          if (!(newChild fastEquals arg)) {
            changed = true
            newChild
          } else {
            arg
          }
        case other => other
      }
      case nonChild: AnyRef => nonChild
      case null => null
    }.toArray
    if (changed) makeCopy(newArgs) else this
  }

  /**
   * Returns a copy of this node where `rule` has been recursively applied first to all of its
   * children and then itself (post-order). When `rule` does not apply to a given node, it is left
   * unchanged.
   * @param rule the function use to transform this nodes children
   */
  def transformUp(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val afterRuleOnChildren = transformChildren(rule, (t, r) => t.transformUp(r))
    if (this fastEquals afterRuleOnChildren) {
      rule.applyOrElse(this, identity[BaseType])
    } else {
      rule.applyOrElse(afterRuleOnChildren, identity[BaseType])
    }
  }

  /**
   * Creates a copy of this type of tree node after a transformation.
   * Must be overridden by child classes that have constructor arguments
   * that are not present in the productIterator.
   * @param newArgs the new product arguments.
   */
  def makeCopy(newArgs: Array[AnyRef]): BaseType = {
    val ctors = getClass.getConstructors.filter(_.getParameterTypes.size != 0)
    if (ctors.isEmpty) {
      sys.error(s"No valid constructor for $nodeName")
    }
    val defaultCtor = ctors.maxBy(_.getParameterTypes.size)

    try {
      // Skip no-arg constructors that are just there for kryo.
      if (otherCopyArgs.isEmpty) {
        defaultCtor.newInstance(newArgs: _*).asInstanceOf[BaseType]
      } else {
        defaultCtor.newInstance((newArgs ++ otherCopyArgs).toArray: _*).asInstanceOf[BaseType]
      }
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throw new TreeNodeException(
          this,
          s"""
             |Failed to copy node.
             |Is otherCopyArgs specified correctly for $nodeName.
             |Exception message: ${e.getMessage}
             |ctor: $defaultCtor?
             |args: ${newArgs.mkString(", ")}
           """.stripMargin)
    }
  }

  def treeString: String = generateTreeString(0, new StringBuilder).toString

  protected def generateTreeString(depth: Int, builder: StringBuilder): StringBuilder = {
    builder.append(" " * depth)
    builder.append(simpleString)
    builder.append("\n")
    children.foreach(_.generateTreeString(depth + 1, builder))
    builder
  }

  def simpleString: String = s"$nodeName $argString".trim
  
  def argString: String = productIterator.flatMap {
    case tn: TreeNode[_] if containsChild(tn) => Nil
    case tn: TreeNode[_] if tn.toString contains "\n" => s"(${tn.simpleString})" :: Nil
    case seq: Seq[BaseType] if seq.toSet.subsetOf(children.toSet) => Nil
    case seq: Seq[_] => seq.mkString("[", ",", "]") :: Nil
    case set: Set[_] => set.mkString("{", ",", "}") :: Nil
    case other => other :: Nil
  }.mkString(", ")

}



  
