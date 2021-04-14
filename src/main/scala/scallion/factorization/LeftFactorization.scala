package scallion
package factorization

/** Contains functions to apply left factorization to a syntax.
  *
  * @groupname factorization Factorization
  */
trait LeftFactorization extends Split { self: Syntaxes with SyntaxesProperties => 

  import Syntax._

  private case class Factorization[A, L](
    factorized: Syntax[L => A],
    alternative: Syntax[A]
  ){
    def |(that: Factorization[A, L]): Factorization[A, L] = {
      Factorization(
        this.factorized | that.factorized,
        this.alternative | that.alternative
      )
    }

    def ~[F](follow: Syntax[F]): Factorization[A ~ F, L] = {
      Factorization(
        (this.factorized ~ follow).map(
            _ match { case fl ~ r => (l: L) => scallion.~(fl(l), r)},
            _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
        ),
        this.alternative ~ follow
      )
    }

    def prepend[P](prefix: Syntax[P]): Factorization[P ~ A, L] = {
      Factorization(
        (prefix ~ this.factorized).map(
          _ match { case l ~ fr => (r: L) => scallion.~(l, fr(r)) },
          _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
        ),
        prefix ~ this.alternative
      )
    }

    def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B, L] = {
      Factorization(
        this.factorized.map(
          factorized => (l: L)  => fun(factorized(l)),
          factorized => throw new IllegalArgumentException("Reverse transformation not yet implemented")
        ),
        this.alternative.map(fun, inv)
      )
    }

    def asRecursive: Factorization[A, L] = {
      def mkRec[B](s: Syntax[B]) = s match {
        case Failure()  => s
        case _          => recursive(s)
      }

      Factorization(
        mkRec(this.factorized),
        mkRec(this.alternative)
      )
    }

    def complete(leftFactor: Syntax[L]): Syntax[A] = {
      (leftFactor ~ this.factorized).map(
        _ match { case l ~ f => f(l) },
        (_: A) => throw new IllegalArgumentException("Reverse transformation not yet implemented")
      ) | this.alternative
    }
  }

  private object Factorization {
    def success[L]: Factorization[L, L] = {
      Factorization(
        Success(()).map( 
          (_: Unit) => (l: L) => {
            l
          }
        ),
        failure
      )
    }

    def fail[A, L](s: Syntax[A]): Factorization[A, L] = {
      Factorization(
        failure,
        s
      )
    }
  }

  // recTerm indicates if recursives should be considered as units
  private def internal[A, L](leftFactor: Syntax[L], s: Syntax[A], recTerm: Boolean): Factorization[A, L] = {
    def iter[A](s: Syntax[A]): Factorization[A, L] = {
      s match {
        case _ if s == leftFactor             => Factorization.success.asInstanceOf[Factorization[A, L]]
        case Elem(_)                          => Factorization.fail(s)
        case Sequence(l, r)                   => {
          val lIter = iter(l)
          if(getProperties(lIter.alternative).isNullable){
            val (lNotNullPart, lNullPart) = splitNullable(lIter.alternative)
            (Factorization(lIter.factorized, lNotNullPart) ~ r) | iter(r).prepend(lNullPart)
          }
          else{
            lIter ~ r
          }
        }
        case Disjunction(l, r)                => iter(l) | iter(r)
        case Transform(fun, inv, inner)       => iter(inner).map(fun, inv)
        case Marked(mark, inner)              => iter(inner) // mark ignored
        case s: Success[_]                    => Factorization.fail(s)
        case Failure()                        => Factorization(failure, failure)
        case Recursive(_, inner) if recTerm   => iter(inner).asRecursive
        case Recursive(_, _) if !recTerm      => Factorization.fail(s)
      }
    }

    iter(s)
  }

  /** Left factorizes a syntax in the syntax.
    * 
    * @param leftFactor Syntax to left factor out.
    * @param s Syntax on which to apply the factorization.
    * @return An equivalent syntax, with the syntax left factorized.
    *
    * @group factorization
    */
  def leftFactorize[A, L](leftFactor: Syntax[L], s: Syntax[A]): Syntax[A] = {
    internal(leftFactor, s, true).complete(leftFactor)
  }

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
  
  /** Remove (factor out) a prefix from a syntax.
    * 
    * @param leftFactor Syntax to left factor out.
    * @param s Syntax on which to apply the factorization.
    * @param recTerm Whether recursive nodes should be broken or not.
    * @return The alternative of the syntax where the prefix could be left factored, and
    *   another syntax where the prefix could not be removed.
    * 
    * @group factorization
    */
  def leftFactorOut[A, L](leftFactor: Syntax[L], s: Syntax[A], recTerm: Boolean = true): (Syntax[L => A], Syntax[A]) = {
    val r = internal(leftFactor, s, recTerm)
    (r.factorized, r.alternative)
  }

}