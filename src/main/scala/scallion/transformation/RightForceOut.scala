package scallion
package transformation

import scala.collection.mutable.Set
import scallion.properties.LL1Properties

/** Contains functions to apply right transformation to a syntax.
  *
  * @groupname transformation Transformation
  */
trait RightForceOut extends Split { self: Syntaxes with Parsers with LL1Properties => 

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

    def succeeded = this.factorized != failure
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

  private def internal[A, R](rightFactor: Syntax[R], s: Syntax[A]): Factorization[A, R] = {
    val recs: Set[Syntax[_]] = Set()
    def visit(s: Syntax[_]): Boolean = {
      val visited = recs.contains(s)
      recs += s
      visited
    }

    def iter[A](s: Syntax[A]): Factorization[A, R] = {
      s match {
        case _ if s eq rightFactor              => Factorization.success.asInstanceOf[Factorization[A, R]]
        case Elem(_)                            => Factorization.fail(s)
        case Sequence(l, r)                     => {
          val lres = iter(l)
          val rres = iter(r)
          if(rres.succeeded){
            rres.prepend(l)
          }
          else if(lres.succeeded){
            val (rNullPart, rNotNullPart) = splitNullable(r)
            val f = lres ~ rNullPart
            Factorization(f.factorized, f.alternative | (l ~ rNotNullPart))
          }
          else{
            Factorization.fail(s)
          }
        }
        case Disjunction(l, r)                  => iter(l) | iter(r)
        case Transform(fun, inv, inner)         => iter(inner).map(fun, inv)
        case Marked(mark, inner)                => iter(inner).mark(mark)
        case s: Success[_]                      => Factorization.fail(s)
        case Failure()                          => Factorization(failure, failure)
        case Recursive(id, inner) if !visit(s)  => iter(inner).asRecursive(id)
        case Recursive(_, _) if visit(s)        => Factorization.fail(s)
      }
    }

    iter(s)
  }

  /////////////////////
  //   Interface     //
  /////////////////////

  /**
    * (Weird) variant of right factor out, where
    * - Syntaxes are compared by reference;
    * - The factored syntax is quaranted to be a rightmost subtree (up to nullable right-siblings).
    *
    * @param rightFactor Syntax to right factor out.
    * @param syntax Syntax on which to apply the transformation.
    * @return The alternative of the syntax where the prefix could be right factored, and
    *   another syntax where the prefix could not be removed.
    */
  def rightForceOut[A, R](rightFactor: Syntax[R], syntax: Syntax[A]): (Syntax[R => A], Syntax[A]) = {
    val res = internal(rightFactor, syntax)
    (res.factorized, res.alternative)
  }
}