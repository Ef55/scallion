package scallion
package factorization

/** Contains functions to apply unfolding to a syntax.
  *
  * @groupname factorization Factorization
  */
trait Unfold { self: Syntaxes => 

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
    * @group factorization
    */
  def unfoldDisjunctions[A](s: Syntax[A]): Syntax[A] = {

    case class Factorization[A](
      alternatives: Seq[Syntax[A]]
    ){
      require(this.alternatives.size > 0)

      def |(that: Factorization[A]): Factorization[A] = {
        Factorization(
          this.alternatives ++ that.alternatives
        )
      }

      def ~[F](that: Factorization[F]): Factorization[A ~ F] = {
        Factorization(
          for(s1 <- this.alternatives;
              s2 <- that.alternatives
          )
            yield s1 ~ s2
          
        )
      }

      def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B] = {
        Factorization(
          this.alternatives.map(s => s.map(fun, inv))
        )
      }

      def asRecursive: Factorization[A] = {
        Factorization(
          this.alternatives.map(s => recursive(s))
        )
      }

      def complete: Syntax[A] = {
        this.alternatives.reduce((s1: Syntax[A], s2: Syntax[A]) => s1 | s2)
      }

      def meaningless: Boolean = this.alternatives.length == 1

      def discard(original: Syntax[A]): Factorization[A] = {
        if(this.meaningless){
          Factorization(original)
        }
        else{
          this
        }
      }

      def unionDiscard(that: Factorization[A], original: Syntax[A]): Factorization[A] = {
        if(this.meaningless && that.meaningless){
          Factorization(original)
        }
        else{
          this | that
        }
      }
    }

    object Factorization {
      def apply[A](s: Syntax[A]): Factorization[A] = {
        Factorization(Seq(s))
      } 
    }

    def unfoldIter[A](s: Syntax[A], topLevel: Boolean): Factorization[A] = {
      s match {
        case e: Elem                        => Factorization(e)
        case Sequence(l, r)                 => (unfoldIter(l, false) ~ unfoldIter(r, false)).discard(s)
        case Disjunction(l, r) if topLevel  => unfoldIter(l, true).unionDiscard(unfoldIter(r, true), s)
        case Disjunction(l, r) if !topLevel => unfoldIter(l, false) | unfoldIter(r, false)
        case Transform(fun, inv, inner)     => unfoldIter(inner, false).map(fun, inv).discard(s)
        case Marked(mark, inner)            => unfoldIter(inner, false).discard(s)
        case Success(_)                     => Factorization(s)
        case Failure()                      => Factorization(s)
        case rec: Recursive[_]              => Factorization(rec)
      }
    }

    unfoldIter(s, true).complete
  }
}