package scallion
package factorization

trait LeftRecursion extends LeftFactorization with Substitution with Unfold with properties.LeftRecursion { self: Syntaxes with SyntaxesProperties =>
  import Syntax._

  def eliminateDirectLeftRecursion[A](syntax: Recursive[A]): Recursive[A] = {
    val (recPart, simplePart) = leftFactorOut(syntax, syntax.inner)
    val factorized = recursive( (simplePart ~ many(recPart)).map{
      case scallion.~(base, follows) => follows.foldLeft(base)((current, follow) => follow(current))
    })

    eliminate(factorized, syntax, factorized).asInstanceOf[Recursive[A]]
  }

  def eliminateLeftRecursion[A](syntax: Recursive[A]): Recursive[A] = {
    if(!isLeftRecursive(syntax)){
      syntax
    }
    else{
      val direct = eliminateDirectLeftRecursion(syntax)
      if(!isLeftRecursive(direct)){
        direct
      }
      else{
        eliminateLeftRecursion(unfoldLeftmostRecursives(direct).asInstanceOf[Recursive[A]])
      }
    }
  }

}