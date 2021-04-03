package scallion

trait SyntaxesNavigation { self: Syntaxes =>
  trait PathNode[A, B] {
    def apply(syntax: Syntax[A]): Syntax[B]
  }

  object PathNode {
    case class Transform[A, B](
      function: A => B,
      inverse: B => Seq[A]) 
    extends PathNode[A, B] {
      def apply(syntax: Syntax[A]) = Syntax.Transform(function, inverse, syntax)
    }

    case class Marked[A](mark: Mark) extends PathNode[A, A]{
      def apply(syntax: Syntax[A]) = Syntax.Marked(mark, syntax)
    }

    case class LeftSequence[L, R](right: Syntax[R]) extends PathNode[L, L~R]{
      def apply(syntax: Syntax[L]) = Syntax.Sequence(syntax, right)
    }

    case class RightSequence[L, R](left: Syntax[L]) extends PathNode[R, L~R]{
      def apply(syntax: Syntax[R]) = Syntax.Sequence(left, syntax)
    }

    case class LeftDisjuction[A](right: Syntax[A]) extends PathNode[A, A]{
      def apply(syntax: Syntax[A]) = Syntax.Disjunction(syntax, right)
    }

    case class RightDisjuction[A](left: Syntax[A]) extends PathNode[A, A]{
      def apply(syntax: Syntax[A]) = Syntax.Disjunction(left, syntax)
    }
  }

  trait Path[A, B] {
    def length: Int
  }
  case class PathCons[A, B, C](head: PathNode[A, B], tail: Path[B, C]) extends Path[A, C] {
    lazy val length: Int = tail.length + 1
  }
  case class NilPath[A]() extends Path[A, A] {
    def length: Int = 0
  }

  case class Zipper[F, T] private(focus: Syntax[F], path: Path[F, T]){
    import PathNode._

    def up: Zipper[_, T] = path match {
      case PathCons(head, tail) => Zipper(head(focus), tail)
      case NilPath()            => throw new IllegalStateException("Cannot go up from top !")
    }

    def zipUp: Syntax[T] = 
      if(path.length > 0){ this.up.zipUp }else{ focus.asInstanceOf[Syntax[T]] }

    def down: Zipper[_, T] = focus match {
      case Syntax.Transform(fun, inv, inner: Syntax[tA]) =>
        Zipper[tA, T](inner, PathCons(Transform(fun ,inv), path))
      case Syntax.Marked(mark, inner) =>
        Zipper[F, T](inner, PathCons(Marked(mark), path))
      case _ =>
        throw new IllegalStateException("Cannot go down from here !")
    }

    // Why on god's green earth is such a hack needed ?!?
    def downLeft: Zipper[_, T] = Zipper.downLeftInternal(this)
    def downRight: Zipper[_, T] = Zipper.downRightInternal(this)
  }

  object Zipper {
    import PathNode._

    def apply[A](syntax: Syntax[A]): Zipper[A, A] = Zipper(syntax, NilPath())

    private def downLeftInternal[F, T](zipper: Zipper[F, T]): Zipper[_, T] = {
      val Zipper(focus, path) = zipper
      focus match {
        case Syntax.Sequence(l: Syntax[tF], r) =>
          Zipper[tF, T](l, PathCons(LeftSequence(r), path))
        case Syntax.Disjunction(l, r) =>
          Zipper[F, T](l, PathCons(LeftDisjuction(r), path))
        case _ =>
          throw new IllegalStateException("Cannot go down-left from here !")
      }
    }

    private def downRightInternal[F, T](zipper: Zipper[F, T]): Zipper[_, T] = {
      val Zipper(focus, path) = zipper
      focus match {
        case Syntax.Sequence(l, r: Syntax[tF]) =>
          Zipper[tF, T](r, PathCons(RightSequence(l), path))
        case Syntax.Disjunction(l, r) =>
          Zipper[F, T](r, PathCons(RightDisjuction(l), path))
        case _ =>
          throw new IllegalStateException("Cannot go down-right from here !")
      }
    }
  }

}