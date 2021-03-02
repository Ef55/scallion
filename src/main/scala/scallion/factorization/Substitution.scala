package scallion
package factorization

import scala.unchecked
import scala.collection.immutable.Set

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
    * @param recSubst Indicates whether the substitutions must be done recursively (see example).
    *
    * @example 
    * {{{
    * lazy val grammar: Syntax[Boolean]   = recursive( epsilon(true) | grammar )
    * lazy val result: Syntax[Boolean]    = recursive( epsilon(false) | grammar )
    * lazy val resultRec: Syntax[Boolean] = recursive( epsilon(false) | result )
    * substitute(grammar, epsilon(true), epsilon(false), false) === result
    * substitute(grammar, epsilon(true), epsilon(false), true)  === resultRec
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
    * If you're not sure about what you are doing, use [[scallion.factorization.Substitution.substituteSafe]] instead, 
    * which will reject calls with error-prone syntaxes.
    *
    * @group factorization
    */
  def substitute[A, B](in: Syntax[B], original: Syntax[A], subst: => Syntax[A], recSubst: Boolean = true): Syntax[B] = {
    
    def iter[C](current: Syntax[C], recs: Set[RecId]): Syntax[C] = {
      // Most of the redundant code could be factorized into a generic function,
      // but the type system + type erasure won't allow it.
      current match {
        case _ if current == original     => subst.asInstanceOf[Syntax[C]]
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
        case rec: Recursive[C @unchecked] => {  // The type is needed to avoid type mismatch when recursing
          if(recs.contains(rec.id)){
            rec
          }
          else{
            val nInner = iter(rec.inner, recs + rec.id)
            if(nInner != rec.inner){ 
              if(recSubst){
                lazy val result: Syntax[C] = recursive( substitute(nInner, rec, result, true) )
                result
              }
              else {
                recursive(nInner)
              }
            }
            else { 
              rec 
            }
          }
        }
      }
    }

    iter(in, Set())
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

  /** Substitutes a Syntax with another one.
   * Behaves like [[scallion.factorization.Substitution.substitute]] but performs additional check
   * to prevent use of syntaxes which might result in unexpected behavior.
   *
   * @see See [[scallion.factorization.Substitution.substitute]].
   * @group factorization
   */
  def substituteSafe[A, B](in: Syntax[B], original: Syntax[A], subst: => Syntax[A], recSubst: Boolean = true): Syntax[B] = {
    assert(isSafeForSubstitution(original), "The syntax to substitute must be safe against equality.")
    substitute(in, original, subst, recSubst)
  }
}