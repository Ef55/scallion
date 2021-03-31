package scallion
package factorization

trait LeftRecursion extends LeftFactorization with Substitution { self: Syntaxes with SyntaxesProperties =>
  import Syntax._

  def eliminateDirectLeftRecursion[A](syntax: Recursive[A]): Syntax[A] = {
    val (recPart, simplePart) = leftFactorOut(syntax, syntax.inner)
    val factorized = (simplePart ~ many(recPart)).map{
      case scallion.~(base, follows) => follows.foldLeft(base)((current, follow) => follow(current))
    }

    eliminate(factorized, syntax, factorized)
  }

}