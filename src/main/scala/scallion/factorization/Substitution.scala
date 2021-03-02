package scallion
package factorization

import scala.collection.immutable.Set

trait Substitution { self: Syntaxes => 

  import Syntax._

  // Behavior can be surprising due to function's equality semantic...
  def substitute[A, B](in: Syntax[B], original: Syntax[A], substitute: Syntax[A]): Syntax[B] = {
    
    def iter[C](current: Syntax[C], recs: Set[RecId]): Syntax[C] = {
      // Most of the redundant code could be factorized into a generic function,
      // but the type system + type erasure won't allow it.
      current match {
        case _ if current == original     => substitute.asInstanceOf[Syntax[C]]
        case e: Elem                      => e 
        case s: Success[_]                => s 
        case f: Failure[_]                => f 

        case s@Sequence(l, r)             => {
          val (nl, nr) = (iter(l, recs), iter(r, recs))
          if(nl != l || nr != r){ nl ~ nr }else{ s }
        }
        case d@Disjunction(l, r)          => {
          val (nl, nr) = (iter(l, recs), iter(r, recs))
          if(nl != l || nr != r){ nl | nr }else{ d }
        }

        case t@Transform(fun, inv, inner) => {
          val nInner = iter(inner, recs)
          if(nInner != inner){ nInner.map(fun, inv) }else{ t }
        }
        case m@Marked(mark, inner)        => {
          val nInner = iter(inner, recs)
          if(nInner != inner){ nInner.mark(mark) }else{ m }
        }
        case rec: Recursive[_]            => {
          if(recs.contains(rec.id)){
            rec
          }
          else{
            val nInner = iter(rec.inner, recs + rec.id)
            if(nInner != rec.inner){ recursive(nInner) }else{ rec }
          }
        }
      }
    }

    iter(in, Set())
  }

  def isSafeForSubstitution(s: Syntax[_]): Boolean = {
    s match {
      case Elem(_)              => true
      case Failure()            => true
      case Success(_)           => true
      case _: Recursive[_]      => true

      case Sequence(l, r)       => isSafeForSubstitution(l) && isSafeForSubstitution(r)
      case Disjunction(l, r)    => isSafeForSubstitution(l) && isSafeForSubstitution(r)
      case Marked(_, i)         => isSafeForSubstitution(i)

      case Transform(_, _, i)   => false
    }
  }

  def substituteSafe[A, B](in: Syntax[B], original: Syntax[A], subs: Syntax[A]): Syntax[B] = {
    assert(isSafeForSubstitution(original), "The syntax to substitute must be safe against equality.")
    substitute(in, original, subs)
  }
}