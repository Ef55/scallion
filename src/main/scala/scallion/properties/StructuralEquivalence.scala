package scallion
package properties

import scala.collection.mutable.{Set, HashMap, MultiMap}

trait StructuralEquivalence { self: Syntaxes =>

  import Syntax._

  def structurallyEquivalent(lhs: Syntax[_], rhs: Syntax[_]): Boolean = {
    val recs = new HashMap[RecId, Set[RecId]] with MultiMap[RecId, RecId]

    def iter(lhs: Syntax[_], rhs: Syntax[_]): Boolean = {
      (lhs, rhs) match {
        case (Elem(e1), Elem(e2)) => e1 == e2
        case (Sequence(l1, r1), Sequence(l2, r2)) =>
          iter(l1, l2) && iter(r1, r2)
        case (Disjunction(l1, r1), Disjunction(l2, r2)) =>
          iter(l1, l2) && iter(r1, r2)
        case (Transform(_, _, i1), Transform(_, _, i2)) => iter(i1, i2)
        case (Marked(_, i1), Marked(_, i2)) => iter(i1, i2)
        case (Success(s1), Success(s2)) => s1 == s2
        case (r1: Recursive[_], r2: Recursive[_]) => {
          if(recs.entryExists(r1.id, _ == r2.id)){
            true
          }
          else{
            recs.addBinding(r1.id, r2.id)
            iter(r1.inner, r2.inner)
          }
        }
        case _ => false
      }
    }

    iter(lhs, rhs)
  }

}