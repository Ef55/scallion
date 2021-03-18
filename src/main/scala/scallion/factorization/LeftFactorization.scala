package scallion
package factorization

/** Contains functions to apply left factorization to a syntax.
  *
  * @groupname factorization Factorization
  */
trait LeftFactorization { self: Syntaxes with SyntaxesProperties => 

  import Syntax._

  /** Left factorizes a single terminal in the syntax.
    * 
    * @param leftFactor Terminal to left factor out.
    * @param s Syntax on which to apply the factorization.
    * @return An equivalent syntax, with the terminal factorized.
    *
    * @group factorization
    */
  def leftFactorize[A](leftFactor: Kind, s: Syntax[A]): Syntax[A] = {
    leftFactorize(Elem(leftFactor), s)
  }

  /** Left factorizes a syntax in the syntax.
    * 
    * @param leftFactor Syntax to left factor out.
    * @param s Syntax on which to apply the factorization.
    * @return An equivalent syntax, with the syntax left factorized.
    *
    * @todo Handle nullable left-handside of [[scallion.Syntaxes.Syntax.Sequence]].
    * @todo Handle recursive syntaxes (through recursive substitution - 
    *       see [[scallion.factorization.Substitution.substitute]]).
    *
    * @group factorization
    */
  def leftFactorize[A, L](leftFactor: Syntax[L], s: Syntax[A]): Syntax[A] = {

    case class Factorization[A](
      factorized: Syntax[L => A],
      alternative: Syntax[A]
    ){
      def |(that: Factorization[A]): Factorization[A] = {
        Factorization(
          this.factorized | that.factorized,
          this.alternative | that.alternative
        )
      }

      def ~[F](follow: Syntax[F]): Factorization[A ~ F] = {
        Factorization(
          (this.factorized ~ follow).map(
              _ match { case fl ~ r => (l: L) => scallion.~(fl(l), r)},
              _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
          ),
          this.alternative ~ follow
        )
      }

      def prepend[P](prefix: Syntax[P]): Factorization[P ~ A] = {
        Factorization(
          (prefix ~ this.factorized).map(
            _ match { case l ~ fr => (r: L) => scallion.~(l, fr(r)) },
            _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
          ),
          prefix ~ this.alternative
        )
      }

      def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B] = {
        Factorization(
          this.factorized.map(
            factorized => (l: L)  => fun(factorized(l)),
            factorized => throw new IllegalArgumentException("Reverse transformation not yet implemented")
          ),
          this.alternative.map(fun, inv)
        )
      }

      def mark(mark: Mark): Factorization[A] = {
        Factorization(
          this.factorized.mark(mark),
          this.alternative.mark(mark)
        )
      }

      def asRecursive: Factorization[A] = {
        Factorization(
          recursive(this.factorized),
          recursive(this.alternative)
        )
      }

      def complete: Syntax[A] = {
        (leftFactor ~ this.factorized).map(
          _ match { case l ~ f => f(l) },
          (_: A) => throw new IllegalArgumentException("Reverse transformation not yet implemented")
        ) | this.alternative
      }
    }

    object Factorization {
      def success: Factorization[L] = {
        Factorization(
          Success(()).map( 
            (_: Unit) => (l: L) => {
              l
            }
          ),
          failure
        )
      }

      def fail[A](s: Syntax[A]): Factorization[A] = {
        Factorization(
          failure,
          s
        )
      }
    }

    def leftFactorOut[A](s: Syntax[A]): Factorization[A] = {
      s match {
        case _ if s == leftFactor       => Factorization.success.asInstanceOf[Factorization[A]]
        case e: Elem                    => Factorization.fail(e)
        case Sequence(l, r)             => {
          val p = getProperties(l)
          if(p.isNull){
            leftFactorOut(r).prepend(l)
          }
          else{
            // If the syntax is nullable, it might be possible
            // that we should factorize both parts; we don't do this
            // in order to stay simple (and because this case implies
            // a first-follow conflict)
            leftFactorOut(l) ~ r
          }
        }
        case Disjunction(l, r)          => leftFactorOut(l) | leftFactorOut(r)
        case Transform(fun, inv, inner) => leftFactorOut(inner).map(fun, inv)
        case Marked(mark, inner)        => leftFactorOut(inner).mark(mark)
        case s: Success[_]              => Factorization.fail(s)
        case Failure()                  => Factorization(failure, failure)
        case rec: Recursive[_]          => leftFactorOut(rec.inner).asRecursive
      }
    }

    leftFactorOut(s).complete
  }

}