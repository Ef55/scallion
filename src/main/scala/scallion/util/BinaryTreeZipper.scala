package scallion.util

import scala.collection.immutable.Set

object BinaryTreeZipper { 
  private def setInclIf[T](set: Set[T], condElements: (Boolean, T)*): Set[T] = {
    condElements.foldLeft(set)( (s, p) => if(p._1){ s + p._2 }else{ s } )
  }

  /** Represent a node in the path from root to focus. */
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
  /** Contains the possible path nodes. */
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

  /** The different directions which can be used to navigate in the tree. */
  sealed trait Direction
  /** Contains all possible directions. */
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
    */
  sealed trait BinaryTreeEquivalent[T] {
    import BinaryTreeEquivalent._

    final def map[A](leaf: A, cons: A, fork: A): A = this match {
      case Leaf(_)        => leaf
      case Cons(_, _)     => cons
      case Fork(_, _, _)  => fork
    }
  }
  /** Contains the binary tree equivalents. */
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
    */
  trait BinaryTreeConvertible[T] {
    def apply(value: T): BinaryTreeEquivalent[T]
  }

  /** A zipper on a binary tree.
    * 
    * @tparam T The type of a node of the binary-tree-like structure.
    * 
    * @groupname result Result
    * @groupname navigation Tree navigation
    * @groupname focus Focus properties
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
    def zipUp: S = 
      if(isRoot){ transform(focus) }else{ this.up.zipUp }

    /** Add a function to apply after zipping
      * the syntax up. 
      *
      * @param fun The function to apply.
      * 
      * @group result
      */
    def map[R](fun: S => R): Zipper[T, R] = Zipper(focus, transform.andThen(fun), path)

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
  }

  /** Factory for zipper. */
  object Zipper {
    private def apply[T, S]
    (focus: T, transform: T => S, path: List[PathNode[T]])
    (implicit convert: BinaryTreeConvertible[T]) = new Zipper(focus, transform, path)

    /** Create a Zipper. */
    def apply[T](focus: T)(implicit convert: BinaryTreeConvertible[T]): Zipper[T, T] = Zipper(focus, x => x, Nil)
  }
}