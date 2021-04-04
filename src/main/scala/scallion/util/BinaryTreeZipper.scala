package scallion.util

import scala.collection.immutable.Set

object BinaryTreeZipper { 
  private def setInclIf[T](set: Set[T], condElements: (Boolean, T)*): Set[T] = {
    condElements.foldLeft(set)( (s, p) => if(p._1){ s + p._2 }else{ s } )
  }

  sealed trait PathNode[T] {
    def up(value: T): T
    def left(value: T): (T, PathNode.LeftNode[T]) = {
      if(!hasLeft)
        throw new UnsupportedOperationException("This node doesn't have a left sibling.")
      else
        throw new IllegalStateException("`left` should have been overloaded by concrete class.")
    }
    def right(value: T): (T, PathNode.RightNode[T]) = {
      if(!hasLeft)
        throw new UnsupportedOperationException("This node doesn't have a left sibling.")
      else
        throw new IllegalStateException("`left` should have been overloaded by concrete class.")
    }
    def hasLeft: Boolean
    def hasRight: Boolean
  }

  object PathNode {
    trait UnaryNode[T] extends PathNode[T] {
      def hasLeft: Boolean = false
      def hasRight: Boolean = false
    }

    trait LeftNode[T] extends PathNode[T] {
      def hasLeft: Boolean = false
      def hasRight: Boolean = true
    }

    trait RightNode[T] extends PathNode[T] {
      def hasLeft: Boolean = true
      def hasRight: Boolean = false
    }
  }

  sealed trait Direction
  object Direction {
    object Up extends Direction
    object Left extends Direction
    object Right extends Direction
    object DownLeft extends Direction
    object Down extends Direction
    object DownRight extends Direction
    val AllDirections: Set[Direction] = Set(Up, Down, DownLeft, DownRight, Left, Right)
  }
  import Direction._

  sealed case class TreeNodeKind(leaf: Boolean, unary: Boolean, binary: Boolean) {
    def isLeaf: Boolean = leaf
    def isUnary: Boolean = unary
    def isBinary: Boolean = binary

    def map[A](leafV: A, unaryV: A, binaryV: A): A = 
      if(leaf) { leafV }
      else if(unary) { unaryV }
      else { binaryV }
  }
  object TreeNodeKind {
    object LeafKind extends TreeNodeKind(true, false, false)
    object UnaryKind extends TreeNodeKind(false, true, false)
    object BinaryKind extends TreeNodeKind(false, false, true)
  }
  import TreeNodeKind._

  trait BinaryBreakable[T] {
    def kind(value: T): TreeNodeKind

    def breakDown(value: T): Option[(T, PathNode.UnaryNode[T])]
    def breakLeft(value: T): Option[(T, PathNode.LeftNode[T])]
    def breakRight(value: T): Option[(T, PathNode.RightNode[T])]
  }

  case class Zipper[T] private(focus: T, path: List[PathNode[T]])(implicit breaker: BinaryBreakable[T]){
    def this(focus: T)(implicit breaker: BinaryBreakable[T]) {
      this(focus, Nil)
    }

    private def illegalMove(move: String): Nothing = throw new IllegalStateException(s"Cannot go ${move} from here !")

    def isTop: Boolean = path.length == 0

    def zipUp: T = 
      if(isTop){ focus }else{ this.up.zipUp }


    // Navigation

    def up: Zipper[T] = path match {
      case head :: tail => Zipper(head.up(focus), tail)
      case Nil          => illegalMove("up")
    }

    def left: Zipper[T] = path match {
      case head :: tail if head.hasLeft => {
        val (left, newHead) = head.left(focus)
        Zipper(left, newHead :: tail)
      }
      case _ => illegalMove("left")
    }
    def right: Zipper[T] = path match {
      case head :: tail if head.hasRight => {
        val (left, newHead) = head.right(focus)
        Zipper(left, newHead :: tail)
      }
      case _ => illegalMove("right")
    }

    def down: Zipper[T] = {
      val (down, newHead) = 
        breaker.breakDown(focus).getOrElse(illegalMove("down"))
      Zipper(down, newHead :: path)
    }
    def downLeft: Zipper[T] = {
      val (down, newHead) = 
        breaker.breakLeft(focus).getOrElse(illegalMove("down-left"))
      Zipper(down, newHead :: path)
    }
    def downRight: this.type = {
      val (down, newHead) = 
        breaker.breakRight(focus).getOrElse(illegalMove("down-right"))
      Zipper(down, newHead :: path).asInstanceOf[this.type]
    }

    def move(directions: List[Direction]): Zipper[T] = {
      def iter(direction: Direction): Zipper[T] = direction match {
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

    def move(directions: Direction*): Zipper[T] = {
      this.move(directions.toList)
    }

    def validDownDirections: Set[Direction] = breaker.kind(focus).map(Set.empty, Set(Down), Set(DownLeft, DownRight))
    def validLateralDirections: Set[Direction] = 
      path.headOption
        .map(head => setInclIf(Set.empty, (head.hasLeft, Left), (head.hasRight, Right)))
        .getOrElse(Set.empty)
    def validDirections: Set[Direction] = setInclIf(validDownDirections ++ validLateralDirections, (!isTop, Up))
    def invalidDirections: Set[Direction] = AllDirections.diff(validDirections)
  }

  object Zipper {
    def apply[T](focus: T)(implicit breaker: BinaryBreakable[T]) = new Zipper(focus)
  }
}