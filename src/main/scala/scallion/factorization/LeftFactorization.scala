package scallion
package factorization

trait LeftFactorization { self: Syntaxes => 

  import Syntax._

  def leftFactorize[A](leftFactor: Kind, s: Syntax[A]): Syntax[A] = {

    case class Factorization[A](
      factorized: Syntax[Token => A],
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
              _ match { case fl ~ r => (t: Token) => scallion.~(fl(t), r)},
              _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
          ),
          this.alternative ~ follow
        )
      }

      def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B] = {
        Factorization(
          this.factorized.map(
            factorized => (t: Token)  => fun(factorized(t)),
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
        (elem(leftFactor) ~ this.factorized).map(
          _ match { case t ~ f => f(t) },
          (_: A) => throw new IllegalArgumentException("Reverse transformation not yet implemented")
        ) | this.alternative
      }
    }

    object Factorization {
      def success: Factorization[Token] = {
        Factorization(
          Success(()).map( 
            (_: Unit) => (t: Token) => {
              require(getKind(t) == leftFactor)
              t
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

    
    // TODO: 
    //  - handle things like leftFactor(k, eps ~ elem(k))
    //  - generalize to left factor whole syntaxes
    def leftFactorOut[A](s: Syntax[A]): Factorization[A] = {
      s match {
        case Elem(`leftFactor`)         => Factorization.success
        case e: Elem                    => Factorization.fail(e)
        case Sequence(l, r)             => leftFactorOut(l) ~ r
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