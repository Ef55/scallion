package scallion
package factorization

trait LeftFactorization { self: Syntaxes => 

  import Syntax._

  def leftFactor[A](leftFactor: Kind, s: Syntax[A]): Syntax[A] = {

    def leftFactorOut[A](leftFactor: Kind, s: Syntax[A]): Syntax[Token => A] = {
      s match {
          case Elem(`leftFactor`)         => Success(()).map( 
            (_: Unit) => (t: Token) => {
              require(getKind(t) == leftFactor)
              t
            }
          )
          case Elem(_)                    => throw new IllegalArgumentException("Non-factorizable not yet handled")

          case Sequence(l, r)             => (leftFactorOut(leftFactor, l) ~ r).map(
            _ match { case fl ~ r => (t: Token) => scallion.~(fl(t), r)},
            _ match { case _ => throw new IllegalArgumentException("Reverse transformation not yet implemented") }
          )

          case Disjunction(l, r)          => leftFactorOut(leftFactor, l) | leftFactorOut(leftFactor, r)
          
          case Transform(fun, inv, inner) => leftFactorOut(leftFactor, inner).map(
            factored => (t: Token)  => fun(factored(t)),
            factored => throw new IllegalArgumentException("Reverse transformation not yet implemented")
          )

          case Marked(mark, inner)        => Marked(mark, leftFactorOut(leftFactor, inner))

          case Success(_)                 => throw new IllegalArgumentException("Succes cannot be factored")

          case Failure()                  => failure

          case rec: Recursive[_]          => recursive(leftFactorOut(leftFactor, rec.inner))
      }
    }

    (elem(leftFactor) ~ leftFactorOut(leftFactor, s)).map(
      _ match { case t ~ f => f(t) },
      _ => throw new IllegalArgumentException("Reverse transformation not yet implemented")
    )
  }

}