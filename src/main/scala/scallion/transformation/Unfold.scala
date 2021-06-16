package scallion
package transformation

import scallion.properties.LL1Properties

/** Contains functions to apply unfolding to a syntax.
  *
  * @groupname transformation Transformation
  */
trait Unfold { self: Syntaxes with Parsers with LL1Properties => 

  import Syntax._

  /** Unfolds disjunctions inside the syntax so that
    * disjunctions only appear at "the top-level" of the syntax.
    *
    * @param s Syntax to unfold.
    *
    * @example
    * {{{
    * val s = a ~ (a | b)
    * unfold(s) === (a ~ a) | (a ~ b)
    * }}}
    *
    * @group transformation
    */
  def unfoldDisjunctions[A](s: Syntax[A]): Syntax[A] = {

    case class Transformation[A](
      alternatives: Seq[Syntax[A]]
    ){
      require(this.alternatives.size > 0)

      def |(that: Transformation[A]): Transformation[A] = {
        Transformation(
          this.alternatives ++ that.alternatives
        )
      }

      def ~[F](that: Transformation[F]): Transformation[A ~ F] = {
        Transformation(
          for(s1 <- this.alternatives;
              s2 <- that.alternatives
          )
            yield s1 ~ s2
          
        )
      }

      def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Transformation[B] = {
        Transformation(
          this.alternatives.map(s => s.map(fun, inv))
        )
      }

      def asRecursive: Transformation[A] = {
        Transformation(
          this.alternatives.map(s => recursive(s))
        )
      }

      def complete: Syntax[A] = {
        this.alternatives.reduce((s1: Syntax[A], s2: Syntax[A]) => s1 | s2)
      }

      // Indicates whether the unfolding is really changing the syntax
      // this is done in an effort to avoid generating new syntaxes
      // which are identical to other ones, which would then be unequal due to transforms
      def meaningless: Boolean = this.alternatives.length == 1

      def discard(original: Syntax[A]): Transformation[A] = {
        if(this.meaningless){
          Transformation(original)
        }
        else{
          this
        }
      }

      def unionDiscard(that: Transformation[A], original: Syntax[A]): Transformation[A] = {
        if(this.meaningless && that.meaningless){
          Transformation(original)
        }
        else{
          this | that
        }
      }
    }

    object Transformation {
      def apply[A](s: Syntax[A]): Transformation[A] = {
        Transformation(Seq(s))
      } 
    }

    def iter[A](s: Syntax[A], topLevel: Boolean): Transformation[A] = {
      s match {
        case e: Elem                        => Transformation(e)
        case Sequence(l, r)                 => (iter(l, false) ~ iter(r, false)).discard(s)
        case Disjunction(l, r) if topLevel  => iter(l, true).unionDiscard(iter(r, true), s)
        case Disjunction(l, r) if !topLevel => iter(l, false) | iter(r, false)
        case Transform(fun, inv, inner)     => iter(inner, false).map(fun, inv).discard(s)
        case Marked(mark, inner)            => iter(inner, false).discard(s)
        case Success(_)                     => Transformation(s)
        case Failure()                      => Transformation(s)
        case rec: Recursive[_]              => Transformation(rec)
      }
    }

    iter(s, true).complete
  }

  /** Unfold all the recursives inside a syntax which might
    * be the first syntax to consume a token.
    *
    * @param syntax Syntax to unfold.
    *
    * @group transformation
    */
  def unfoldLeftmostRecursives[A](syntax: Syntax[A]): Syntax[A] = {
    def iter[B](syntax: Syntax[B]): Syntax[B] = syntax match {
      case Elem(_)                        => syntax
      case Sequence(l, r)                 => 
        if(isNullable(l)){
          iter(l) ~ iter(r)
        }
        else{
          iter(l) ~ r
        }
      case Disjunction(l, r)              => iter(l) | iter(r)
      case Transform(fun, inv, inner)     => {
        val unfInner = iter(inner)
        if(unfInner == inner){
          syntax
        }
        else{
          unfInner.map(fun, inv)
        }
      }
      case Marked(mark, inner)            => iter(inner).mark(mark)
      case Success(_)                     => syntax
      case Failure()                      => syntax
      case Recursive(_, inner)            => inner
    } 

    iter(syntax)
  }
}