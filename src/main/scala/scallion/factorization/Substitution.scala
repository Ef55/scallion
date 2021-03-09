package scallion
package factorization

import scala.collection.mutable.HashMap

/** Contains functions to apply substitution to a syntax.
  *
  * @groupname factorization Factorization
  */
trait Substitution { self: Syntaxes => 

  import Syntax._

  /** Substitutes a Syntax with another one.
    *
    * @param in Syntax in which the substition(s) will happen.
    * @param original Syntax which will be removed.
    * @param subst Syntax to use as replacement.
    * @param elim Indicates whether `original` must be completely eliminated from returned syntax (see example 2).
    *
    * @example 
    * {{{
    * lazy val grammar: Syntax[Boolean] = recursive( epsilon(true) | grammar )
    * lazy val result: Syntax[Boolean] = recursive( epsilon(false) | result )
    * substitute(grammar, epsilon(true), epsilon(false))  === result
    * }}}
    *
    * @example
    * {{{
    * lazy val grammar: Syntax[Boolean] = recursive( (epsilon(true) ~ grammar).map{ case a ~ b => a && b} )
    * lazy val substitute: Syntax[Boolean] = recursive( (epsilon(false) ~ grammar).map{ case a ~ b => a || b} )
    * substitute(grammar, grammar, substitute, false) === substitute
    * substitute(grammar, grammar, substitute, true) 
    * === recursive( (epsilon(false) ~ substitute).map{ case a ~ b => a || b} )
    * }}}
    *
    * @note Due to some implementation details, this function might return unexpected results
    *       when working with mapped syntaxes:
    * {{{
    * val transform = (c: Char) => c.asDigit
    * val grammar = epsilon('1').map(transform) ~ epsilon(2)
    * val expected = epsilon(3) ~ epsilon(2)
    * substitute(grammar, epsilon('1').map(transform), epsilon(3))
    * === grammar   // Left unchanged...
    * =!= expected  // ... which is not what we expected.
    * }}}
    *
    * @group factorization
    */
  def substitute[A, B](in: Syntax[B], original: Syntax[A], subst: Syntax[A], elim: Boolean = true): Syntax[B] = {
    val substs = HashMap[Syntax[_], Syntax[_]]()
    substs += (original -> (if(elim){ iter(subst) }else{ subst })) 

    def iter[C](current: Syntax[C]): Syntax[C] = {
      // Most of the redundant code could be factorized into a generic function,
      // but the type system + type erasure won't allow it.
      current match {
        case _ if substs.contains(current)=> substs.get(current).get.asInstanceOf[Syntax[C]]
        case e: Elem                      => e 
        case s: Success[_]                => s 
        case f: Failure[_]                => f 

        case s@Sequence(l, r)             => {
          val (nl, nr) = (iter(l), iter(r))
          if(nl != l || nr != r){ nl ~ nr }else{ s }
        }
        case d@Disjunction(l, r)          => {
          val (nl, nr) = (iter(l), iter(r))
          if(nl != l || nr != r){ nl | nr }else{ d }
        }

        case t@Transform(fun, inv, inner) => {
          val nInner = iter(inner)
          if(nInner != inner){ nInner.map(fun, inv) }else{ t }
        }
        case m@Marked(mark, inner)        => {
          val nInner = iter(inner)
          if(nInner != inner){ nInner.mark(mark) }else{ m }
        }
        case rec: Recursive[_] => {
          substs += ( current -> recursive(iter(rec.inner)) )
          substs.get(current).get.asInstanceOf[Syntax[C]]
        }
      }
    }

    iter(in)
  }


  /**
    * @todo Replace by isSafeForEquality
    */
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
}