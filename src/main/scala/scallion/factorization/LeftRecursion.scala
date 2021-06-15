package scallion
package factorization

trait LeftRecursion extends LeftFactorization 
with Substitution with Unfold with SyntaxesNavigation with properties.Recursion { self: Syntaxes with SyntaxesProperties =>
  import Syntax._

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