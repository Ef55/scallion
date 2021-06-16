package scallion.factorization

import scallion.Syntaxes
import scallion.SyntaxesProperties
import scala.annotation.tailrec

/** Contains functions to factorize syntaxes. 
 * 
 * @groupname conflicts Conflicts resolution
 */
trait Factorization extends LeftFactorization with Substitution with Unfold with Split {
  self: Syntaxes with SyntaxesProperties => 

  import Conflict._

  private def solveFirstConflict[A](syntax: Syntax[A], conflict: FirstConflict): Syntax[A] = {
    val tokens = conflict.ambiguities
    // The present cast doesn't have any meaning; it is just required in order to typecheck
    val src = conflict.source.asInstanceOf[Syntax[Any]] 
    tokens.foldLeft(syntax)( (s, t) => eliminate(s, src, leftFactorizeKind(t, src)))
  }

  /**
    * Attempt to solve first-first conflicts inside the syntax
    *
    * @param syntax The syntax with a conflict.
    * 
    * @group conflicts
    */
  def solveFirstConflicts[A](syntax: Syntax[A]): Syntax[A] = {

    @tailrec
    def iter[B](syntax: Syntax[B], conflicts: List[Conflict]): Syntax[B] = {
      conflicts match {
        case (fc: FirstConflict) :: _ => {
          val newSyntax = solveFirstConflict(syntax, fc)
          iter(newSyntax, getProperties(newSyntax).conflicts.toList)
        }
        case h :: tl => iter(syntax, tl)
        case Nil     => syntax
      }
    }

    iter(syntax, getProperties(syntax).conflicts.toList)
  }
}