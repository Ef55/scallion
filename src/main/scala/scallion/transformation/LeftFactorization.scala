package scallion
package transformation

import scallion.properties.LL1Properties

/** Contains functions to apply left factorization to a syntax.
  *
  * @groupname transformation Transformation
  */
trait LeftFactorization extends Split { self: Syntaxes with Parsers with LL1Properties => 

  /////////////////////
  //   Internals     //
  /////////////////////
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
            _ match { case _ => throw new IllegalArgumentException("Reverse transformation not implemented") }
        ),
        this.alternative ~ follow
      )
    }

    def prepend[P](prefix: Syntax[P]): Factorization[P ~ A, L] = {
      Factorization(
        (prefix ~ this.factorized).map(
          _ match { case l ~ fr => (r: L) => scallion.~(l, fr(r)) },
          _ match { case _ => throw new IllegalArgumentException("Reverse transformation not implemented") }
        ),
        prefix ~ this.alternative
      )
    }

    def map[B](fun: A => B, inv: B => Seq[A] = (b: B) => Seq()): Factorization[B, L] = {
      Factorization(
        this.factorized.map(
          factorized => (l: L)  => fun(factorized(l)),
          factorized => throw new IllegalArgumentException("Reverse transformation not implemented")
        ),
        this.alternative.map(fun, inv)
      )
    }

    def mark(mark: Mark): Factorization[A, L] = {
      Factorization(
        factorized.mark(mark ++ "-Fol"), 
        alternative.mark(mark ++ "-Alt")
      )
    }

    def asRecursive(oldId: RecId): Factorization[A, L] = {
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

    def complete(leftFactor: Syntax[L]): Syntax[A] = {
      (leftFactor ~ this.factorized).map(
        _ match { case l ~ f => f(l) },
        (_: A) => throw new IllegalArgumentException("Reverse transformation not implemented")
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
          if(isNullable(l)){
            val (lNotNullPart, lNullPart) = splitNullable(l, recTerm)
            (iter(lNotNullPart) ~ r) | iter(r).prepend(lNullPart)
          }
          else{
            iter(l) ~ r
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

  /** Left factorizes a syntax in the syntax.
    * 
    * @param leftFactor Syntax to left factor out.
    * @param s Syntax on which to apply the transformation.
    * @param recTerm Whether recursive nodes should be broken or not.
    * @return An equivalent syntax, with the syntax left factorized.
    *
    * @group transformation
    */
  def leftFactorize[A, L](leftFactor: Syntax[L], s: Syntax[A], recTerm: Boolean = true): Syntax[A] = {
    internal(leftFactor, s, recTerm).complete(leftFactor)
  }

  /** Left factorizes a single terminal in the syntax.
  * 
  * @param leftFactor Terminal to left factor out.
  * @param s Syntax on which to apply the transformation.
  * @param recTerm Whether recursive nodes should be broken or not.
  * @return An equivalent syntax, with the terminal factorized.
  *
  * @group transformation
  */
  def leftFactorizeKind[A](leftFactor: Kind, s: Syntax[A], recTerm: Boolean = true): Syntax[A] = {
    leftFactorize(elem(leftFactor), s, recTerm)
  }
  
  /** Remove (factor out) a prefix from a syntax.
    * 
    * @param leftFactor Syntax to left factor out.
    * @param s Syntax on which to apply the transformation.
    * @param recTerm Whether recursive nodes should be broken or not.
    * @return The alternative of the syntax where the prefix could be left factored, and
    *   another syntax where the prefix could not be removed.
    * 
    * @group transformation
    */
  def leftFactorOut[A, L](leftFactor: Syntax[L], s: Syntax[A], recTerm: Boolean = true): (Syntax[L => A], Syntax[A]) = {
    val r = internal(leftFactor, s, recTerm)
    (r.factorized, r.alternative)
  }

}