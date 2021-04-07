package scallion
package properties

import scala.collection.immutable.{Queue, Set}
import scala.collection.mutable.{Set => MSet, HashSet}

trait Recursion extends SyntaxesNavigation { self: Syntaxes with SyntaxesProperties =>
  import Syntax._ 

  /** List all the recursives in the syntax tree.
    * 
    * The recursives are ordered by ascending
    * depth first and then from left to right.
    * 
    * @param syntax The syntax to analyze
    * 
    * @group property
    */
  def listRecursives(syntax: Syntax[_]): List[Recursive[_]] = {
    def isRecursive(syntax: Syntax[_]): Boolean = syntax match {
      case Recursive(_, _)  => true
      case _                => false
    }

    def retrieveRecursives(syntax: Syntax[_]): List[Recursive[_]] = 
      Zipper(syntax).walk.filter(isRecursive).toList.map(_.asInstanceOf[Recursive[_]])

    def iter(queue: Queue[Recursive[_]], result: List[Recursive[_]]): List[Recursive[_]] = {
      queue.dequeueOption match {
        case Some((rec, newQueue)) if !result.contains(rec)  => {
          iter(newQueue ++ retrieveRecursives(rec.inner), rec :: result)
        }
        case Some((_, newQueue))  => iter(newQueue, result)
        case None                 => result.reverse
      }
    }

    iter(Queue.empty ++ retrieveRecursives(syntax), Nil)
  }

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

  /** Indicate whether the syntax contains left-recursive non-terminals.
    *
    * @group property  
    */
  def hasLeftRecursion(syntax: Syntax[_]): Boolean = {
    !findLeftRecursions(syntax).isEmpty
  }

  def isLeftRecursive(syntax: Recursive[_]): Boolean = {
    findLeftRecursions(syntax).contains(syntax)
  }
}