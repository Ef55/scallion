package scallion
package factorization

import java.util.Properties

trait Split { self: SyntaxesProperties with Syntaxes =>
  import Syntax._

  private def optZip[A, B](o1: Option[A], o2: Option[B]): Option[(A, B)] = (o1, o2) match {
    case (Some(a), Some(b)) => Some((a, b))
    case _                  => None
  }

  private def optDisjuction[A](opts: Option[Syntax[A]]*): Option[Syntax[A]] = {
    opts.reduce(
      (o1, o2) => optZip(o1, o2).map(p => p._1 | p._2).orElse(o1).orElse(o2)
    )
  }

  /**
    * Split a (possibly) nullable syntax into a 
    * null syntax (only accepts the empty string)
    * and a non-nullable syntax (doesn't accept the empty string).
    *
    * @param syntax The syantax to split.
    * @return The non-nullable component if any, and the null component if any.
    */
  def splitNullable[A](syntax: Syntax[A]): 
  (Option[Syntax[A]], Option[Syntax[A]]) = {

    val prop = getProperties(syntax)
    if(prop.isNull){
      (None, Some(syntax))
    }
    else if(!prop.isNullable){
      (Some(syntax), None)
    }
    else {
      syntax match {

        case Sequence(l: Syntax[tB], r: Syntax[tC]) => {
          val (nonNulll, nulll) = splitNullable(l)
          val (nonNullr, nullr) = splitNullable(r)
          val comb = (o1: Option[Syntax[tB]], o2: Option[Syntax[tC]]) => optZip(o1, o2).map(p => p._1 ~ p._2)
          (
            optDisjuction(comb(nonNulll, nonNullr), comb(nonNulll, nullr), comb(nulll, nonNullr)),
            comb(nulll, nullr)
          )
        }

        case Disjunction(l, r) => {
          val (nonNulll, nulll) = splitNullable(l)
          val (nonNullr, nullr) = splitNullable(r)
          (optDisjuction(nonNulll, nonNullr), optDisjuction(nulll, nullr))
        }

        case Transform(fun, inv, inner) => {
          val (nonNull, nul) = splitNullable(inner)
          (nonNull.map(_.map(fun, inv)), nul.map(_.map(fun, inv)))
        }

        case Marked(_, inner) => splitNullable(inner)

        case Recursive(_, inner) => {
          val (nonNull, nul) = splitNullable(inner)
          (nonNull.map(recursive(_)), nul.map(recursive(_)))
        }
      }
    }
  }

}