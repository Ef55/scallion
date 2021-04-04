package scallion

import scala.collection.immutable.Set
import scallion.util.BinaryTreeZipper

trait SyntaxesNavigation { self: Syntaxes =>
  import Syntax._

  val Direction = BinaryTreeZipper.Direction
  import Direction._

  private object SyntaxesPathNode {
    import BinaryTreeZipper.PathNode._

    case class Transform[A, B](
      function: A => B,
      inverse: B => Seq[A]) 
    extends UnaryNode[Syntax[_]] {
      override def up(syntax: Syntax[_]) = Syntax.Transform(function, inverse, syntax.asInstanceOf[Syntax[A]])
      override def hasLeft: Boolean = false
      override def hasRight: Boolean = false
    }

    case class Marked[A](mark: Mark) extends UnaryNode[Syntax[_]]{
      override def up(syntax: Syntax[_]) = Syntax.Marked(mark, syntax.asInstanceOf[Syntax[A]])
      override def hasLeft: Boolean = false
      override def hasRight: Boolean = false
    }

    case class LeftSequence[L, R](right: Syntax[R]) extends LeftNode[Syntax[_]]{
      override def up(syntax: Syntax[_]) = Syntax.Sequence(syntax.asInstanceOf[Syntax[L]], right)
      override def hasLeft: Boolean = false
      override def hasRight: Boolean = true
    }

    case class RightSequence[L, R](left: Syntax[L]) extends RightNode[Syntax[_]]{
      override def up(syntax: Syntax[_]) = Syntax.Sequence(left, syntax.asInstanceOf[Syntax[R]])
      override def hasLeft: Boolean = true
      override def hasRight: Boolean = false
    }

    case class LeftDisjuction[A](right: Syntax[A]) extends LeftNode[Syntax[_]]{
      override def up(syntax: Syntax[_]) = Syntax.Disjunction(syntax.asInstanceOf[Syntax[A]], right)
      override def hasLeft: Boolean = false
      override def hasRight: Boolean = true
    }

    case class RightDisjuction[A](left: Syntax[A]) extends RightNode[Syntax[_]]{
      override def up(syntax: Syntax[_]) = Syntax.Disjunction(left, syntax.asInstanceOf[Syntax[A]])
      override def hasLeft: Boolean = true
      override def hasRight: Boolean = false
    }
  }
  
  type Zipper = BinaryTreeZipper.Zipper[Syntax[_]]

  object Zipper {
    import BinaryTreeZipper.PathNode._

    def apply[A](syntax: Syntax[A]) = new Zipper(syntax)(syntaxesBreakable)

    private val syntaxesBreakable = new BinaryTreeZipper.BinaryBreakable[Syntax[_]]  {
      import BinaryTreeZipper.TreeNodeKind
      import BinaryTreeZipper.TreeNodeKind._

      def kind(value: Syntax[_]): TreeNodeKind = value match {
        case Elem(_)            => LeafKind
        case Success(_)         => LeafKind
        case Failure()          => LeafKind
        case Transform(_, _, _) => UnaryKind
        case Marked(_, _)       => UnaryKind
        case Sequence(_, _)     => BinaryKind
        case Disjunction(_, _)  => BinaryKind
        case Recursive(_, _)    => LeafKind
      }

      def breakDown(value: Syntax[_]): Option[(Syntax[_], UnaryNode[Syntax[_]])] = value match {
        case Transform(fun, inv, inner) => Some((inner, SyntaxesPathNode.Transform(fun, inv)))
        case Marked(mark, inner)        => Some((inner, SyntaxesPathNode.Marked(mark)))
        case _                          => None
      }
      def breakLeft(value: Syntax[_]): Option[(Syntax[_], LeftNode[Syntax[_]])] = value match {
        case Sequence(l, r)     => Some((l, SyntaxesPathNode.LeftSequence(r)))
        case Disjunction(l, r)  => Some((l, SyntaxesPathNode.LeftDisjuction(r)))
        case _                  => None
      }
      def breakRight(value: Syntax[_]): Option[(Syntax[_], RightNode[Syntax[_]])] = value match {
        case Sequence(l, r)     => Some((r, SyntaxesPathNode.RightSequence(l)))
        case Disjunction(l, r)  => Some((r, SyntaxesPathNode.RightDisjuction(l)))
        case _                  => None
      }

    }
  }
}