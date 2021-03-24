package scallion
package properties

import scala.collection.immutable.Set
import scala.collection.mutable.{Set => MSet, HashSet}

/** Contains functions to detect left recursion in a syntax.
  *
  * @groupname property Syntaxes property
  */
trait LeftRecursion { self: Syntaxes with SyntaxesProperties =>
  import Syntax._

  /**
    * Search if the syntax contains any left-recursive
    * non-terminal, and return the left-recursives if so.
    * 
    * @param syntax The syntax to analyze.
    * @return The left-recursive non-terminals.
    *
    * @group property
    */
  def findLeftRecursions(syntax: Syntax[_]): Set[Recursive[_]] = {
    val analyzed: MSet[Recursive[_]] = new HashSet()


    def iter(syntax: Syntax[_], leftmost: Boolean, recs: Set[Syntax[_]]): Set[Recursive[_]] = syntax match {
      case Elem(_)                => Set.empty
      case Success(_)             => Set.empty
      case Failure()              => Set.empty
      case Sequence(l, r)         => iter(l, leftmost, recs) ++ iter(r, leftmost && getProperties(l).isNullable, recs)
      case Disjunction(l, r)      => iter(l, leftmost, recs) ++ iter(r, leftmost, recs)
      case Transform(_, _, inner) => iter(inner, leftmost, recs)
      case Marked(_, inner)       => iter(inner, leftmost, recs)
      case rec: Recursive[_]      => {
        val subResult: Set[Recursive[_]] = if(recs.contains(rec)){
          if(leftmost){ Set(rec) }else{ Set.empty }
        }
        else{
          iter(rec.inner, leftmost, recs + rec)
        }
        
        if(!analyzed.contains(rec) && !subResult.contains(rec)){
          // Restart analysis with this recursive as root
          analyzed += rec
          subResult ++ iter(rec, true, Set.empty)
        }
        else{
          subResult
        }
      }
    }

    iter(syntax, true, Set.empty)
  }

  /** @return If the syntax contains left-recursive non-terminals.
    *
    * @group property  
    */
  def hasLeftRecursion(syntax: Syntax[_]): Boolean = {
    !findLeftRecursions(syntax).isEmpty
  }
}