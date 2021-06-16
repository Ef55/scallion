package scallion
package transformation

import scallion.properties.LL1Properties
import scala.collection.immutable.Map
import scala.collection.mutable.HashMap

/** Contains functions to apply substitution to a syntax.
  *
  * @groupname transformation Transformation
  */
trait Substitution extends properties.Recursion { self: Syntaxes with Parsers with LL1Properties => 

  import Syntax._

  private def ifRec[U](s: Syntax[_], f: Recursive[_] => U, other: U): U = s match{
    case r: Recursive[_]  => f(r)
    case _                => other
  }

  /** Substitutes a syntax with another one.
    *
    * @param in Syntax in which the substition(s) will happen.
    * @param original Syntax which will be removed.
    * @param subst Syntax to use as replacement.
    * @param elim Indicates whether `original` must also be substituted inside of `subst` (see example 2).
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
    * lazy val subst: Syntax[Boolean] = recursive( (epsilon(false) ~ grammar).map{ case a ~ b => a || b} )
    * lazy val eliminate: Syntax[Boolean] = recursive( (epsilon(false) ~ eliminate).map{ case a ~ b => a || b} )
    * substitute(grammar, grammar, subst) === subst
    * eliminate(grammar, grammar, subst) === eliminate
    * }}}
    *
    * @note Due to some implementation details, this function might return unexpected results
    *       when working with mapped syntaxes (see [[scallion.properties.StructuralEquivalence]]):
    * {{{
    * val transform = (c: Char) => c.asDigit
    * val grammar = epsilon('1').map(transform) ~ epsilon(2)
    * val expected = epsilon(3) ~ epsilon(2)
    * substitute(grammar, epsilon('1').map(transform), epsilon(3))
    * === grammar   // Left unchanged...
    * =!= expected  // ... which is not what we expected.
    * }}}
    *
    * @group transformation
    */
  def substitute[A, B](in: Syntax[A], original: Syntax[B], subst: Syntax[B]): Syntax[A] = {
    val map: Map[Syntax[_], Syntax[_]] = Map((original, subst))
    substitute(in, map, false)
  }

  /** Substitutes a syntax with another one.
    *
    * Unlike the base version, this one allows to perform multiple
    * substitutions at once, given by the substitution map.
    * 
    * @param in Syntax in which the substition(s) will happen.
    * @param substitutions Mappings old -> new syntax 
    * @param elim Indicates whether `original` must also be substituted inside of `subst`.
    *
    * @group transformation
    */
  def substitute[A](in: Syntax[A], substitutions: Map[Syntax[_], Syntax[_]], elim: Boolean = false): Syntax[A] = {

    trait LazySyntax { def syntax[A]: Syntax[A] }
    object LazySyntax{
      def apply(s: => Syntax[_]) = new LazySyntax {
        lazy val syntaxVal = s
        override def syntax[A] = syntaxVal.asInstanceOf[Syntax[A]]
      }
    }

    val substs = HashMap[Syntax[_], LazySyntax]()
    for(s <- substitutions.filter(p => p._1 != p._2)){
      substs += (s._1 -> LazySyntax(
        if(elim){ iter(s._2) }
        else{ s._2 }
      )) 
    }

    def iter[C](current: Syntax[C]): Syntax[C] = {
      // Most of the redundant code could be factorized into a generic function,
      // but the type system + type erasure won't allow it.
      current match {
        case _ if substs.contains(current)=> substs.get(current).get.syntax[C]
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
        case Recursive(id, inner)          => {
          val newRec = recursive(iter(inner)).asInstanceOf[Recursive[_]]
          substs += ( current -> LazySyntax(newRec) )
          substs.get(current).get.syntax[C]
        }
      }
    }

    iter(in)
  }

  /** Substitutes a syntax with another one.
    *
    * This version just add an additional way to specify the substitution mappings.
    * 
    * @group transformation
    **/
  def substitute[A](in: Syntax[A], substitutions: (Syntax[_], Syntax[_])*): Syntax[A] = 
    substitute(in, substitutions.toMap, false)

  /** Eliminate syntaxex by substituting it with other ones.
    *
    * This function is just a shorthand for substitute with
    * `elim = true`.
    * 
    * @group transformation
    **/
  def eliminate[A](in: Syntax[A], substitutions: Map[Syntax[_], Syntax[_]]): Syntax[A] = 
    substitute(in, substitutions, true)

  /** Eliminate syntaxex by substituting it with other ones.
    *
    * This function is just a shorthand for substitute with
    * `elim = true`.
    * 
    * @group transformation
    **/
  def eliminate[A](in: Syntax[A], substitutions: (Syntax[_], Syntax[_])*): Syntax[A] = 
    substitute(in, substitutions.toMap, true)

  /** Eliminate a Syntax by substituting it with another one.
    *
    * This function is just a shorthand for substitute with
    * `elim = true`.
    * {{{
    * substitute(i, o, s, true) === eliminate(i, o, s)
    * }}}
    * 
    * @param in Syntax in which the substition(s) will happen.
    * @param original Syntax to eliminate.
    * @param subst Syntax to use as replacement.
    * 
    * @see [[scallion.transformation.Substitution.substitute[A,B]*]]
    * 
    * @group transformation
    */
  def eliminate[A, B](in: Syntax[B], original: Syntax[A], subst: Syntax[A]): Syntax[B] = {
    val map: Map[Syntax[_], Syntax[_]] = Map((original, subst))
    substitute(in, map, true)
  }

}