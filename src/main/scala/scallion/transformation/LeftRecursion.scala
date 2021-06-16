package scallion
package transformation

import scallion.properties.LL1Properties

/** Contains functions to eliminate left recursion.
  *
  * @groupname transformation Transformation
  */
trait LeftRecursion extends LeftFactorization 
with Substitution with Unfold with SyntaxesNavigation with properties.Recursion { self: Syntaxes with Parsers with LL1Properties =>
  import Syntax._

  /////////////////////
  //   Internals     //
  /////////////////////
  private def eliminateDirectLeftRecursionUntyped(syntax: Recursive[_]): Syntax[_] = {
    eliminateDirectLeftRecursion(syntax)
  }

  private[scallion] def eliminateDirectLeftRecursions[A](syntax: Syntax[A]): Syntax[A] = {
    val map = listRecursives(syntax)
        .map(rec => ( rec, eliminateDirectLeftRecursionUntyped(rec) ))
        .toMap[Syntax[_], Syntax[_]]
    eliminate(
      syntax, 
      map
    )
  }

  private[scallion] def unfoldLeftmostRecursivesOfLeftRecursives[A](syntax: Syntax[A]): Syntax[A] = {
    listRecursives(syntax).foreach(r => assert(!isDirectLeftRecursive(r), r.id))
    val map = listRecursives(syntax)
        .filter(r => isLeftRecursive(r))
        .take(1)
        .map(rec => (
            rec, 
            recursive( unfoldLeftmostRecursives(rec.inner) )
          ))
        .toMap[Syntax[_], Syntax[_]]
    eliminate(syntax, map)
  }

  private[scallion] def eliminateLeftRecursionsStep[A](syntax: Syntax[A]): Syntax[A] = {
    unfoldLeftmostRecursivesOfLeftRecursives(eliminateDirectLeftRecursions(syntax))
  }

  /////////////////////
  //   Interface     //
  /////////////////////

  /** Eliminate direct left recursion from a syntax.
    *
    * This transforms syntaxes unsuited for LL1 parsing such as
    * {{{
    * lazy val rec: Syntax[Seq[Boolean]] = recursive{ rec :+ (elem(false) | eps(true)) }
    * }}}
    * into non direct left recursive ones.
    * 
    * @param syntax Syntax to transform.
    * 
    * @group transformation
    */
  def eliminateDirectLeftRecursion[A](syntax: Recursive[A]): Syntax[A] = {
    if(isDirectLeftRecursive(syntax)){
      val (recPart, simplePart) = leftFactorOut(syntax, syntax.inner, false)
      val rec = many(recPart)
      val factorized = recursive { 
        (simplePart ~ rec).map{
          case scallion.~(base, follows) => follows.foldLeft(base)((current, follow) => follow(current))
        }
      }

      factorized
    }
    else{
      syntax
    }
  }

  /** Full procedure to eliminate all left recursions from a syntax.
    * 
    * @param syntax Syntax to transform.
    * 
    * @group transformation
    */
  def eliminateLeftRecursions[A](syntax: Syntax[A]): Syntax[A] = {
    def iter(s: Syntax[A]): Syntax[A] = {
      if(!hasLeftRecursion(s)){
        s
      }
      else{
        iter(eliminateLeftRecursionsStep(s))
      }
    }
    iter(syntax)
  }


}