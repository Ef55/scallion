package scallion

import scala.collection.immutable.Set
import scallion.util.BinaryTreeZipper

trait SyntaxesNavigation { self: Syntaxes =>
  import Syntax._

  /**@see [[scallion.util.BinaryTreeZipper.Direction]] */
  val Direction = BinaryTreeZipper.Direction
  import Direction._
  
  /** A zipper on a syntax. */
  type Zipper[A] = BinaryTreeZipper.Zipper[Syntax[_], Syntax[A]]
  type Walk[A] = BinaryTreeZipper.Walk[Syntax[_], Syntax[A]]

  /** Factory for syntax zipper. */
  object Zipper {
    import BinaryTreeZipper.BinaryTreeEquivalent
    import BinaryTreeZipper.BinaryTreeEquivalent._

    def apply[A](syntax: Syntax[A]) = 
      BinaryTreeZipper.Zipper[Syntax[_]](syntax)(syntaxesConverter)
      .map(_.asInstanceOf[Syntax[A]])

    private val syntaxesConverter = new BinaryTreeZipper.BinaryTreeConvertible[Syntax[_]]  {
      def apply(syntax: Syntax[_]): BinaryTreeEquivalent[Syntax[_]] = syntax match {
        case Elem(_) | Success(_) | Failure() =>
          Leaf(syntax)
        case Transform(fun, inv, inner: Syntax[tA]) => 
          Cons(inner, (s: Syntax[_]) => Transform(fun, inv, s.asInstanceOf[Syntax[tA]]).asInstanceOf[Syntax[_]])
        case Marked(mark, inner) =>
          Cons(inner, Marked(mark, _))
        case Sequence(l, r) =>
          Fork(l, r, Sequence(_, _))
        case Disjunction(l: Syntax[tA], r) =>
          Fork(l, r, (left: Syntax[_], right: Syntax[_]) => Disjunction(left.asInstanceOf[Syntax[tA]], right.asInstanceOf[Syntax[tA]]))
        case Recursive(_, _) =>
          Leaf(syntax)
      }
    }
  }

  object Walk {
    def postOrder[A](zipper: Zipper[A]): Walk[A] = zipper.walkPostOrder
    def postOrder[A](syntax: Syntax[A]): Walk[A] = postOrder(Zipper(syntax))
    def preOrder[A](zipper: Zipper[A]): Walk[A] = zipper.walkPreOrder
    def preOrder[A](syntax: Syntax[A]): Walk[A] = preOrder(Zipper(syntax))
  }
}