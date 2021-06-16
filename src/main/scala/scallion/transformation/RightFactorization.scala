package scallion
package transformation

import scallion.properties.LL1Properties

/** Contains functions to apply right factorization to a syntax.
  *
  * @groupname transformation Transformation
  */
trait RightFactorization extends Split { self: Syntaxes with Parsers with LL1Properties => 

  /////////////////////
  //   Internals     //
  /////////////////////
  import Syntax._

  private case class Factorization[A, R](
    factorized: Syntax[R => A],
    alternative: Syntax[A]
  ){
    def |(that: Factorization[A, R]): Factorization[A, R] = {
      Factorization(
        this.factorized | that.factorized,
        this.alternative | that.alternative
      )
    }

    def ~[F](follow: Syntax[F]): Factorization[A ~ F, R] = {
      Factorization(
        (this.factorized ~ follow).map(
            _ match { case fl ~ r => (l: R) => scallion.~(fl(l), r)},
            _ match { case _ => throw new IllegalArgumentException("Reverse transformation not implemented") }
        ),
        this.alternative ~ follow
      )
    }

    def prepend[P](prefix: Syntax[P]): Factorization[P ~ A, R] = {
      Factorization(
        (prefix ~ this.factorized).map(
          _ match { case l ~ fr => (r: R) => scallion.~(l, fr(r)) },
          _ match { case _ => throw new IllegalArgumentException("Reverse transformation not implemented") }
        ),
        prefix ~ this.alternative
      )
    }

    def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B, R] = {
      Factorization(
        this.factorized.map(
          factorized => (l: R)  => fun(factorized(l)),
          factorized => throw new IllegalArgumentException("Reverse transformation not implemented")
        ),
        this.alternative.map(fun, inv)
      )
    }

    def mark(mark: Mark): Factorization[A, R] = {
      Factorization(
        factorized.mark(mark ++ "-Fol"), 
        alternative.mark(mark ++ "-Alt")
      )
    }

    def asRecursive(oldId: RecId): Factorization[A, R] = {
      def mkRec[B](s: Syntax[B]) = s match {
        case Failure()  => s
        case _          => {
          val newRec = recursive(s).asInstanceOf[Recursive[B]]
          newRec
        }
      }

      Factorization(
        mkRec(this.factorized),
        mkRec(this.alternative)
      )
    }

    def complete(rightFactor: Syntax[R]): Syntax[A] = {
      (this.factorized ~ rightFactor).map(
        _ match { case f ~ r => f(r) },
        (_: A) => throw new IllegalArgumentException("Reverse transformation not implemented")
      ) | this.alternative
    }
  }

  private object Factorization {
    def success[R]: Factorization[R, R] = {
      Factorization(
        Success(()).map( 
          (_: Unit) => (l: R) => {
            l
          }
        ),
        failure
      )
    }

    def fail[A, R](s: Syntax[A]): Factorization[A, R] = {
      Factorization(
        failure,
        s
      )
    }
  }

  // recTerm indicates if recursives should be considered as units
  private def internal[A, R](rightFactor: Syntax[R], s: Syntax[A], recTerm: Boolean): Factorization[A, R] = {
    def iter[A](s: Syntax[A]): Factorization[A, R] = {
      s match {
        case _ if s == rightFactor             => Factorization.success.asInstanceOf[Factorization[A, R]]
        case Elem(_)                          => Factorization.fail(s)
        case Sequence(l, r)                   => {
          if(isNullable(r)){
            val (rNotNullPart, rNullPart) = splitNullable(r, recTerm)
            iter(rNotNullPart).prepend(l) | (iter(l) ~ rNullPart)
          }
          else{
            iter(r).prepend(l)
          }
        }
        case Disjunction(l, r)                => iter(l) | iter(r)
        case Transform(fun, inv, inner)       => iter(inner).map(fun, inv)
        case Marked(mark, inner)              => iter(inner).mark(mark)
        case s: Success[_]                    => Factorization.fail(s)
        case Failure()                        => Factorization(failure, failure)
        case Recursive(id, inner) if recTerm  => iter(inner).asRecursive(id)
        case Recursive(_, _) if !recTerm      => Factorization.fail(s)
      }
    }

    iter(s)
  }

  /////////////////////
  //   Interface     //
  /////////////////////

  /** Right factorizes a syntax in the syntax.
    * 
    * @param rightFactor Syntax to right factor out.
    * @param s Syntax on which to apply the transformation.
    * @param recTerm Whether recursive nodes should be broken or not.
    * @return An equivalent syntax, with the syntax right factorized.
    *
    * @group transformation
    */
  def rightFactorize[A, R](rightFactor: Syntax[R], s: Syntax[A], recTerm: Boolean = true): Syntax[A] = {
    internal(rightFactor, s, recTerm).complete(rightFactor)
  }

  /** Right factorizes a single terminal in the syntax.
  * 
  * @param rightFactor Terminal to right factor out.
  * @param s Syntax on which to apply the transformation.
  * @param recTerm Whether recursive nodes should be broken or not.
  * @return An equivalent syntax, with the terminal factorized.
  *
  * @group transformation
  */
  def rightFactorizeKind[A](rightFactor: Kind, s: Syntax[A], recTerm: Boolean = true): Syntax[A] = {
    rightFactorize(elem(rightFactor), s, recTerm)
  }
  
  /** Remove (factor out) a prefix from a syntax.
    * 
    * @param rightFactor Syntax to right factor out.
    * @param s Syntax on which to apply the transformation.
    * @param recTerm Whether recursive nodes should be broken or not.
    * @return The alternative of the syntax where the prefix could be right factored, and
    *   another syntax where the prefix could not be removed.
    * 
    * @group transformation
    */
  def rightFactorOut[A, R](rightFactor: Syntax[R], s: Syntax[A], recTerm: Boolean = true): (Syntax[R => A], Syntax[A]) = {
    val r = internal(rightFactor, s, recTerm)
    (r.factorized, r.alternative)
  }

}