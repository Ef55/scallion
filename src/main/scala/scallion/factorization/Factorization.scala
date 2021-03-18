package scallion.factorization

import scallion.Syntaxes
import scallion.SyntaxesProperties
import scala.annotation.tailrec

/** Contains functions to factorize syntaxes. */
trait Factorization extends LeftFactorization with Substitution with Unfold {
  self: Syntaxes with SyntaxesProperties => 

  import Conflict._

  def solveFirstConflicts[A](syntax: Syntax[A]): Syntax[A] = {

    @tailrec
    def iter[B](syntax: Syntax[B], conflicts: List[Conflict]): Syntax[B] = {
      def leftFactorizeAndEliminate[C](i: Syntax[B], o: Syntax[C], t: Kind): Syntax[B] = {
        eliminate(i, o, leftFactorize(t, o))
      }

      conflicts match {
        case FirstConflict(src, tokens) :: _ => {
          val newSyntax = leftFactorizeAndEliminate(syntax, src, tokens.head)
          iter(newSyntax, getProperties(newSyntax).conflicts.toList)
        }
        case h :: tl => println(h); iter(syntax, tl)
        case Nil     => syntax
      }
    }

    iter(syntax, getProperties(syntax).conflicts.toList)
  }
}