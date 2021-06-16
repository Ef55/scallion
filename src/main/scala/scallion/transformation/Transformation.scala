package scallion
package transformation

import scallion.properties.LL1Properties
import scala.annotation.tailrec

/** Contains functions to factorize syntaxes. 
 * 
 * @groupname conflicts Conflicts resolution
 */
trait Transformations extends 
  LeftFactorization with RightFactorization with RightForceOut with
  LeftRecursion with Substitution with 
  Unfold with Split with LL1Properties {
  self: Syntaxes with Parsers => 

  import Syntax._
  import Conflict._

  private def solveFirstConflict[A](syntax: Syntax[A], conflict: FirstConflict): Syntax[A] = {
    val tokens = conflict.ambiguities
    // The present cast doesn't have any meaning; it is just required in order to typecheck
    val src = conflict.source.asInstanceOf[Syntax[Any]] 
    tokens.foldLeft(syntax)( (s, t) => eliminate(s, src, leftFactorizeKind(t, src)))
  }

  private def solveFollowConflict[A](syntax: Syntax[A], conflict: FollowConflict): Syntax[A] = {
    val tokens = conflict.ambiguities
    (conflict.source, conflict.root) match {
      case ( src: Syntax[tS], root@Sequence(l: Syntax[tL], r: Syntax[tR]) ) => {
        val (factorized: Syntax[tS => tL], alternative: Syntax[tS]) = rightForceOut(src, l)
        val solved: Syntax[tS ~ tR] = tokens.foldLeft(src ~ r)( (s, t) => leftFactorizeKind(t, s))
        val newSyntax: Syntax[tL ~ tR] = (alternative ~ r) | 
          (factorized ~ solved).map{ case f ~ (l ~ r) => scallion.~(f(l), r) }
        eliminate(syntax, root, newSyntax)
      }
    }
  }

  /**
    * Attempt to solve conflicts inside the syntax
    *
    * @param syntax The syntax with a conflict.
    * 
    * @group conflicts
    */
  def solveConflicts[A](syntax: Syntax[A]): Syntax[A] = {

    @tailrec
    def iter(syntax: Syntax[A], conflicts: List[Conflict]): Syntax[A] = {
      conflicts match {
        case (fc: FirstConflict) :: _ => {
          val newSyntax = solveFirstConflict(syntax, fc)
          startIter(newSyntax)
        }
        case (fc: FollowConflict) :: _ => {
          val newSyntax = solveFollowConflict(syntax, fc)
          startIter(newSyntax)
        }
        case h :: tl => iter(syntax, tl)
        case Nil     => syntax
      }
    }

    def startIter(syntax: Syntax[A]): Syntax[A] = {
      val s = eliminateLeftRecursions(syntax)
      iter(s, getConflicts(s).toList)
    }

    startIter(syntax)
  }
}