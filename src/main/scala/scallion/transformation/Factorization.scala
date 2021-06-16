package scallion
package transformation

import scallion.properties.LL1Properties
import scala.annotation.tailrec

/** Contains functions to factorize syntaxes. 
 * 
 * @groupname conflicts Conflicts resolution
 */
trait Transformation extends LeftFactorization with Substitution with Unfold with Split {
  self: Syntaxes with Parsers with LL1Properties => 

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
          iter(newSyntax, getConflicts(newSyntax).toList)
        }
        case h :: tl => iter(syntax, tl)
        case Nil     => syntax
      }
    }

    iter(syntax, getConflicts(syntax).toList)
  }
}