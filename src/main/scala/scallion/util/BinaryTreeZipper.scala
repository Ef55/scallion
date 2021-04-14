package scallion.util

import scala.collection.immutable.Set

/** A zipper for Binary-Tree-like date structures. 
 * 
 * In order to use this zipper on your won data structure, you should
 * provide an implementation of `BinaryTreeConvertible[T]` where `T`
 * is a common ancestors-type of all your node-types.
 * 
 * @see [[scallion.SyntaxesNavigation]] for an example on how to implement
 * `BinaryTreeConvertible[T]`.
 * 
 * @groupname zipper Zipper
 * @groupname binarytree Binary tree conversion
 * @groupname navigation Zipper navigation
 * @groupname path Root path
 */
object BinaryTreeZipper { 
  private def setInclIf[T](set: Set[T], condElements: (Boolean, T)*): Set[T] = {
    condElements.foldLeft(set)( (s, p) => if(p._1){ s + p._2 }else{ s } )
  }

  /** Represent a node in the path from root to focus. 
    * @group path
    */
  sealed trait PathNode[T] {
    /** Go up one node in the path.
      *
      * @param value The current focus.
      */
    def up(value: T): T

    /** Change the path node to go left instead of right.
      *
      * @param value The current focus.
      * @return The left sibling and the new node.
      */
    def left(value: T): (T, PathNode.LeftNode[T])

    /** Change the path node to right instead of left.
      *
      * @param value The current focus.
      * @return The right sibling and the new node.
      */
    def right(value: T): (T, PathNode.RightNode[T])

    /** Indicate if this path node has a left sibling. */
    def hasLeft: Boolean
    /** Indicate if this path node has a right sibling. */
    def hasRight: Boolean
  }
  /** Contains the possible path nodes. 
    * @group path
    */
  object PathNode {
    /** A path node with only child. */
    final case class UnaryNode[T](cons: T => T) extends PathNode[T] {
      def hasLeft: Boolean = false
      def hasRight: Boolean = false

      def up(value: T): T = cons(value)
      def left(value: T) = throw new UnsupportedOperationException("This node doesn't have a left sibling.")
      def right(value: T) = throw new UnsupportedOperationException("This node doesn't have a right sibling.")
    }

    /** A path node with two child where the path goes to the left. */
    final case class LeftNode[T](right: T, cons: (T, T) => T) extends PathNode[T] {
      def hasLeft: Boolean = false
      def hasRight: Boolean = true

      def up(value: T): T = cons(value, right)
      def left(value: T) = throw new UnsupportedOperationException("This node doesn't have a left sibling.")
      def right(value: T): (T, RightNode[T]) = (right, RightNode(value, cons))
    }

    /** A path node with two child where the path goes to the right. */
    final case class RightNode[T](left: T, cons: (T, T) => T) extends PathNode[T] {
      def hasLeft: Boolean = true
      def hasRight: Boolean = false

      def up(value: T): T = cons(left, value)
      def left(value: T): (T, LeftNode[T]) = (left, LeftNode(value, cons))
      def right(value: T) = throw new UnsupportedOperationException("This node doesn't have a left sibling.")
    }
  }
  import PathNode._

  /** The different directions which can be used to navigate in the tree. 
    * @group navigation
    */
  sealed trait Direction
  /** Contains all possible directions. 
    * @group navigation
    */
  object Direction {
    case object Up extends Direction
    case object Left extends Direction
    case object Right extends Direction
    case object DownLeft extends Direction
    case object Down extends Direction
    case object DownRight extends Direction
    val AllDirections: Set[Direction] = Set(Up, Down, DownLeft, DownRight, Left, Right)
  }
  import Direction._

  /** Information needed on a kind of node of a data structure
    * in order for the zipper to be able to navigate it.
    * 
    * @tparam T The type of a node in the data structure.
    * @group binarytree
    */
  sealed trait BinaryTreeEquivalent[T] {
    import BinaryTreeEquivalent._

    final def map[A](leaf: A, cons: A, fork: A): A = this match {
      case Leaf(_)        => leaf
      case Cons(_, _)     => cons
      case Fork(_, _, _)  => fork
    }
  }
  /** Contains the binary tree equivalents. 
    * @group binarytree
    */
  object BinaryTreeEquivalent {
    /** A leaf node (0 children). */
    final case class Leaf[T](value: T) extends BinaryTreeEquivalent[T]
    /** A unary node (1 children). */
    final case class Cons[T](subtree: T, cons: T => T) extends BinaryTreeEquivalent[T]
    /** A binary node (2 children). */
    final case class Fork[T](left: T, right: T, cons: (T, T) => T) extends BinaryTreeEquivalent[T]
  }
  import BinaryTreeEquivalent._

  /** Transform some data structure into an equivalent 
    * binary tree which will then be used by the zipper.
    * 
    * @tparam T The type of a node in the data structure.
    * @group binarytree
    */
  trait BinaryTreeConvertible[T] {
    def apply(value: T): BinaryTreeEquivalent[T]
  }

  /** A zipper on a binary tree.
    * 
    * @tparam T The type of a node of the binary-tree-like structure.
    * 
    * @group zipper
    * @groupname result Result
    * @groupname navigation Tree navigation
    * @groupname focus Focus properties
    * @groupname transform Transformations
    */
  final class Zipper[T, S] private
  (val focus: T, val transform: T => S, private val path: List[PathNode[T]])
  (implicit convert: BinaryTreeConvertible[T]){

    private def illegalMove(move: String): Nothing = throw new IllegalStateException(s"Cannot go ${move} from here !")


    /** Zip the tree up. 
      * 
      * @return The tree this zipper is navigating.
      * 
      * @group result
      */
    def zipUp: Zipper[T, S] = 
      if(isRoot){ this }else{ this.up.zipUp }

    /** Zip the tree up, apply the transform to the resulting tree and return. */
    def close: S = transform(this.zipUp.focus)

    /** Add a function to apply after zipping
      * the syntax up. 
      *
      * @param fun The function to apply.
      * 
      * @group result
      */
    def map[R](fun: S => R): Zipper[T, R] = Zipper(focus, transform.andThen(fun), path)

    /** Replace the focused node by another one.
      *
      * @param replacement The replacement node.
      * @return The replaced node and the new tree.
      * 
      * @group transform
      */
    def replace(replacement: T): (T, Zipper[T, S]) = 
      (focus, Zipper(replacement, transform, path))

    /** Indicate if the root is focused. 
      * @group focus
      */
    def isRoot: Boolean = path.length == 0

    /** Indicate to which children the focus can be moved to. 
      * @group focus
      */
    def validDownDirections: Set[Direction] = convert(focus).map(Set.empty, Set(Down), Set(DownLeft, DownRight))

    /** Indicate to which sibling the focus can be moved to. 
      * @group focus
      */
    def validLateralDirections: Set[Direction] = 
      path.headOption
        .map(head => setInclIf(Set[Direction](), (head.hasLeft, Left), (head.hasRight, Right)))
        .getOrElse(Set.empty)
    
    /** Indicate where the focus can be moved to. 
      * @group focus
      */
    def validDirections: Set[Direction] = setInclIf(validDownDirections ++ validLateralDirections, (!isRoot, Up))
    
    def validDirections(directions: Direction*): Set[Direction] = validDirections.intersect(directions.toSet)

    def validDirection(direction: Direction): Boolean = validDirections.contains(direction)

    /** Indicate where the focus cannot be moved to. 
      * @group focus
      */
    def invalidDirections: Set[Direction] = AllDirections.diff(validDirections)


    /** Move the focus up (parent).
      * @group navigation
      */
    def up: Zipper[T, S] = path match {
      case head :: tail => Zipper(head.up(focus), transform, tail)
      case Nil          => illegalMove("up")
    }

    /** Move the focus to the left (sibling).
      * @group navigation
      */
    def left: Zipper[T, S] = path match {
      case head :: tail if head.hasLeft => {
        val (left, newHead) = head.left(focus)
        Zipper(left, transform, newHead :: tail)
      }
      case _ => illegalMove("left")
    }

    /** Move the focus to the right (sibling).
      * @group navigation
      */
    def right: Zipper[T, S] = path match {
      case head :: tail if head.hasRight => {
        val (left, newHead) = head.right(focus)
        Zipper(left, transform, newHead :: tail)
      }
      case _ => illegalMove("right")
    }

    /** Move the focus down (only child).
      * @group navigation
      */
    def down: Zipper[T, S] = {
      convert(focus) match {
        case Cons(value, cons)  => Zipper(value, transform, UnaryNode(cons) :: path)
        case _                  => illegalMove("down")
      }
    }

    /** Move the focus down-left (left child).
      * @group navigation
      */
    def downLeft: Zipper[T, S] = {
      convert(focus) match {
        case Fork(l, r, cons)   => Zipper(l, transform, LeftNode(r, cons) :: path)
        case _                  => illegalMove("down-left")
      }
    }

    /** Move the focus down-right (right child).
      * @group navigation
      */
    def downRight: Zipper[T, S] = {
      convert(focus) match {
        case Fork(l, r, cons)   => Zipper(r, transform, RightNode(l, cons) :: path)
        case _                  => illegalMove("down-right")
      }
    }

    /** Move the focus according to directions.
      * @group navigation
      */
    def move(directions: List[Direction]): Zipper[T, S] = {
      def iter(direction: Direction): Zipper[T, S] = direction match {
        case Up         => this.up
        case Left       => this.left
        case Right      => this.right 
        case Down       => this.down
        case DownLeft   => this.downLeft
        case DownRight  => this.downRight
      }

      directions match {
        case head :: tl => iter(head).move(tl)
        case Nil        => this
      }
    }

    /** Move the focus according to directions.
      * @group navigation
      */
    def move(directions: Direction*): Zipper[T, S] = {
      this.move(directions.toList)
    }

    /** Initiate a walk on the currently focused subtree. */
    def walkPostOrder: Walk[T, S] = Walk.postOrder(this)


    /** Initiate a walk on the currently focused subtree. */
    def walkPreOrder: Walk[T, S] = Walk.preOrder(this)
  }

  /** Factory for zipper. 
    * @group zipper
    */
  object Zipper {
    private def apply[T, S]
    (focus: T, transform: T => S, path: List[PathNode[T]])
    (implicit convert: BinaryTreeConvertible[T]) = new Zipper(focus, transform, path)

    /** Create a Zipper. */
    def apply[T](focus: T)(implicit convert: BinaryTreeConvertible[T]): Zipper[T, T] = Zipper(focus, x => x, Nil)
  
  }

  /** Allow to visit every node of a zipper. 
    * @group navigation
    */
  sealed abstract class Walk[T: BinaryTreeConvertible, S](protected val in: Zipper[T, S]) {
    protected def focus: Zipper[T, T]

    /** Restart the current walk.
      * All transformations/filtering are kept for the new walk.
      */
    def restart: Walk[T, S]

    /** The current node in the walk.
      * @return The current node, or None if the walk hasn't started or has ended.
      */
    def current: Option[T]

    /** Continue the walk.
      * @return The next node, or None if there are no more nodes.
      */
    def next: Option[T]

    /** Indicate if the walk ended. */
    def done: Boolean

    /** Replace the current node with another one. */
    def replaceCurrent(replacement: T): T

    /** Apply a transformative function to the current node. */
    final def mapCurrent(replacer: T => T): T = {
      replaceCurrent(replacer(focus.focus))
    }

    /** Apply a transformative function to all the remaining nodes. 
      * 
      * The walk will be done after this operation.
      */
    final def mapNexts(replacer: T => T): Walk[T, S] = {
      while(next.isDefined){
        mapCurrent(replacer)
      }
      this
    }

    /** Apply a transformation to all node, regardless of the
      * current state of the walk.
      * 
      * The walk will be done after this operation.
      */
    final def map(replacer: T => T): Walk[T, S] = {
      restart.mapNexts(replacer)
    }

    /**@return A list of all node remaining in the walk. */
    def toList: List[T]

    /** Return the original zipper this walk was started on. */
    final def cancel: Zipper[T, S] = in

    /** Add a predicate which all future returned node will satisfy. */
    def filter(filter: T => Boolean): Walk[T, S] = new Walk.FilteredWalk(this, filter)

    /** Conclude the walk by returning a zipper with all applied
      * replacements.
      * 
      * @pram forced Indicate if it must be concluded event if some nodes were not yet visited.
      * @throws IllegalStateException If the walk is not done and the conclusion is not forced.
      */
    final def conclude(forced: Boolean): Zipper[T, S]  = 
      if(done || forced){
        in.replace(focus.close)._2
      }
      else{
        throw new IllegalStateException("Cannot finalize a walk in progress !")
      }

    /** Conclude the walk and then close the resulting zipper. */
    def close(forced: Boolean): S = this.conclude(forced).close
    /** Conclude the walk and then close the resulting zipper. */
    def close: S = close(false)

    /** Conclude the walk without forcing it. */
    final def conclude: Zipper[T, S] = conclude(false)

    /** Convert to iterator.
      * @note This operation is not lazy; all elements of this walk
      * will be computed eagerly at call.
      */
    final def toIterator: Iterator[T] = toList.iterator
  }

  /** Factory for Walk. 
    * @group navigation
    */
  object Walk {
    def postOrder[T: BinaryTreeConvertible, S](zipper: Zipper[T, S]): Walk[T, S] = new PostOrderWalk(zipper)
    def postOrder[T: BinaryTreeConvertible](value: T): Walk[T, T] = postOrder(Zipper(value))
    def preOrder[T: BinaryTreeConvertible, S](zipper: Zipper[T, S]): Walk[T, S] = new PreOrderWalk(zipper)
    def preOrder[T: BinaryTreeConvertible](value: T): Walk[T, T] = preOrder(Zipper(value))

    private def canGoUp[T](focus: Zipper[T, T]) = !focus.validDirections(Right, Up).isEmpty

    private def gotoLeftmost[T](focus: Zipper[T, T]): Zipper[T, T] = {
      var newFocus = focus
      var downDir = newFocus.validDirections(DownLeft, Down).headOption
      while(downDir.isDefined){
        newFocus = newFocus.move(downDir.get)
        downDir = newFocus.validDirections(DownLeft, Down).headOption
      }
      newFocus
    }

    private def gotoFirstRight[T](focus: Zipper[T, T]): Option[Zipper[T, T]] = {
      if(focus.validDirection(Right)){
        Some(focus.right)
      }
      else if(focus.validDirection(Up)){
        gotoFirstRight(focus.up)
      }
      else{
        None
      }
    }

    private def gotoNextPreOrder[T](focus: Zipper[T, T], start: Boolean): Option[Zipper[T, T]] = {
      val downDir = focus.validDirections(DownLeft, Down).headOption
      if(start){
        Some(focus)
      }
      else if(downDir.isDefined){
        Some(focus.move(downDir.get))
      }
      else{
        gotoFirstRight(focus)
      }
    }

    private def gotoNextPostOrder[T](focus: Zipper[T, T], start: Boolean): Option[Zipper[T, T]] = 
      if(start){
        Some(gotoLeftmost(focus))
      }
      else if(!canGoUp(focus)){
        None
      }
      else{
        if(focus.validDirection(Right)){
          Some(gotoLeftmost(focus.right))
        }
        else{
          Some(focus.up)
        }
      }


    private sealed abstract class ExtremaOrderWalk[T: BinaryTreeConvertible, S] private
    (private var tree: Zipper[T, T], in: Zipper[T, S])
    extends Walk[T, S](in) { 
      private var startFlag: Boolean = true
      private var doneFlag: Boolean = false

      protected def this(root: Zipper[T, S]) {
        this(Zipper(root.focus), root)
      }

      protected final def focus: Zipper[T, T] = tree
      protected def gotoNext(focus: Zipper[T, T], start: Boolean): Option[Zipper[T, T]]

      override final def restart: ExtremaOrderWalk[T, S] = {
        tree = tree.zipUp
        startFlag = true
        doneFlag = false
        this
      }

      override final def current: Option[T] = if(doneFlag || startFlag){ None }else{ Some(tree.focus) }
      override final def next: Option[T] = { 
        gotoNext(tree, startFlag) match {
          case Some(zipper) => tree = zipper
          case None         => doneFlag = true
        }
        startFlag = false
        current
      }
      override final def done: Boolean = doneFlag

      override final def replaceCurrent(replacement: T): T =
        if(current.isEmpty){
          throw new IllegalStateException("Cannot replace current if there is none !")
        }
        else{
          val (replaced, newTree) = tree.replace(replacement)
          tree = newTree
          replaced
        }

      override final def toList: List[T] = {
        var optNext = gotoNext(tree, startFlag)
        var ls: List[T] = Nil
        while(optNext.isDefined){
          ls = optNext.get.focus :: ls
          optNext = gotoNextPostOrder(optNext.get, false)
        }
        ls.reverse
      }
    }


    private final class PostOrderWalk[T: BinaryTreeConvertible, S] private[scallion]
    (root: Zipper[T, S]) extends ExtremaOrderWalk[T, S](root) { 

      protected def gotoNext(focus: Zipper[T, T], start: Boolean): Option[Zipper[T, T]] = 
        gotoNextPostOrder[T](focus, start)
    }

    private final class PreOrderWalk[T: BinaryTreeConvertible, S] private[scallion]
    (root: Zipper[T, S]) extends ExtremaOrderWalk[T, S](root) { 

      protected def gotoNext(focus: Zipper[T, T], start: Boolean): Option[Zipper[T, T]] = 
        gotoNextPreOrder[T](focus, start)
    }

    private final class FilteredWalk[T: BinaryTreeConvertible, S] private[util]
    (private val inner: Walk[T, S], val filter: T => Boolean)
    extends Walk[T, S](inner.in) { 
      private var startFlag: Boolean = true

      protected def focus: Zipper[T, T] = inner.focus

      def restart: FilteredWalk[T, S] = {
        inner.restart
        this
      }

      def current: Option[T] = if(startFlag){ None }else{ inner.current }
      def next: Option[T] = {
        startFlag = false
        while(!done && inner.next.filter(filter).isEmpty) {}
        current
      }
      def done: Boolean = inner.done

      override def filter(additional: T => Boolean): Walk[T, S] = new FilteredWalk(inner, x => filter(x) && additional(x))

      def replaceCurrent(replacement: T): T = inner.replaceCurrent(replacement)

      override def toList: List[T] = inner.toList.filter(filter)
    }
  }
}