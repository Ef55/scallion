package scallion
package factorization

trait Substitution { self: Syntaxes => 

  import Syntax._

  def substitute[A, B](in: Syntax[B])(original: Syntax[A], substitute: Syntax[A]): Syntax[B] = {
    def iter[C](current: Syntax[C]): Syntax[C] = {
      current match {
        case _ if current == original   => substitute.asInstanceOf[Syntax[C]]
        case e: Elem                    => e 
        case Sequence(l, r)             => iter(l) ~ iter(r)
        case Disjunction(l, r)          => iter(l) | iter(r)
        case Transform(fun, inv, inner) => iter(inner).map(fun, inv)
        case Marked(mark, inner)        => iter(inner).mark(mark)
        case s: Success[_]              => s 
        case f: Failure[_]              => f 
        case rec: Recursive[_]          => recursive(iter(rec.inner))
      }
    }

    iter(in)
  }
}