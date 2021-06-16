package scallion
package transformation

import scallion.properties.LL1Properties

/** Contains functions to transform a syntax into an quivalent
  * disjuction, where each alternative has some specified property.
  *
  * @groupname transformation Transformation
  */
trait Split { self: Syntaxes with Parsers with LL1Properties =>
  import Syntax._

  /////////////////////
  //   Internals     //
  /////////////////////

  private def optZip[A, B](o1: Option[A], o2: Option[B]): Option[(A, B)] = (o1, o2) match {
    case (Some(a), Some(b)) => Some((a, b))
    case _                  => None
  }

  private def optDisjuction[A](opts: Option[Syntax[A]]*): Option[Syntax[A]] = {
    opts.reduce(
      (o1, o2) => optZip(o1, o2).map(p => p._1 | p._2).orElse(o1).orElse(o2)
    )
  }

  /////////////////////
  //   Interface     //
  /////////////////////

  /**
    * Split a (possibly) nullable syntax into a 
    * null syntax (only accepts the empty string)
    * and a non-nullable syntax (doesn't accept the empty string).
    *
    * @note Will not terminate if the syntax is left recursive.
    * 
    * @param syntax The syantax to split.
    * @return The non-nullable component if any, and the null component if any.
    *
    * @group transformation
    */
  def trySplitNullable[A](syntax: Syntax[A], recTerm: Boolean = true): (Option[Syntax[A]], Option[Syntax[A]]) = {

    if(isNull(syntax)){
      (None, Some(syntax))
    }
    else if(!isNullable(syntax)){
      (Some(syntax), None)
    }
    else {
      syntax match {

        case Sequence(l: Syntax[tB], r: Syntax[tC]) => {
          val (nonNulll, nulll) = trySplitNullable(l)
          val (nonNullr, nullr) = trySplitNullable(r)
          val comb = (o1: Option[Syntax[tB]], o2: Option[Syntax[tC]]) => optZip(o1, o2).map(p => p._1 ~ p._2)
          (
            optDisjuction(comb(nonNulll, nonNullr), comb(nonNulll, nullr), comb(nulll, nonNullr)),
            comb(nulll, nullr)
          )
        }

        case Disjunction(l, r) => {
          val (nonNulll, nulll) = trySplitNullable(l)
          val (nonNullr, nullr) = trySplitNullable(r)
          (optDisjuction(nonNulll, nonNullr), optDisjuction(nulll, nullr))
        }

        case Transform(fun, inv, inner) => {
          val (nonNull, nul) = trySplitNullable(inner)
          (nonNull.map(_.map(fun, inv)), nul.map(_.map(fun, inv)))
        }

        case Marked(_, inner) => trySplitNullable(inner)

        case Recursive(_, inner) if recTerm => {
          val (nonNull, nul) = trySplitNullable(inner)
          (nonNull.map(recursive(_)), nul.map(recursive(_)))
        }

        case Recursive(_, _) if !recTerm => (Some(syntax), None)
      }
    }
  }

  def splitNullable[A](syntax: Syntax[A], recTerm: Boolean = true): (Syntax[A], Syntax[A]) = {
    if(!isNullable(syntax)){
      throw new IllegalArgumentException("The syntax should be nullable in order to be split by nullability")
    }
    val r = trySplitNullable(syntax, recTerm)
    (r._1.getOrElse(failure), r._2.getOrElse(failure))
  }

  /**
    * Split a syntax into a part which has a prefix satisfying a
    * given predicate and a part whose prefixes do not satisfy it.
    * 
    * @note Might not terminate if the syntax is left recursive and
    * `enterRecursive` is set to `true`.
    *
    * @param syntax The syntax to split.
    * @param splitter The predicate indicating how to split.
    * @param enterRecursive Whether recursive construct should be split.
    * @return The part whose prefix satisfy the preidcate if any, and the part whose prefix don't if any.
    *
    * @group transformation
    */
  def splitByPrefix[A](syntax: Syntax[A], splitter: Syntax[_] => Boolean, enterRecursive: Boolean = false): 
  (Option[Syntax[A]], Option[Syntax[A]]) = {
    def iter[B](syntax: Syntax[B]): (Option[Syntax[B]], Option[Syntax[B]]) = {
      if(splitter(syntax)){
        (Some(syntax), None)
      }
      else{
        syntax match {
          case Elem(_)      => (None, Some(syntax))
          case Success(_)   => (None, Some(syntax))
          case Failure()    => (None, Some(syntax))

          case Sequence(l, r) => {
            val (prefixed, unprefixed) = iter(l)
            (prefixed.map(_ ~ r), unprefixed.map(_ ~ r))
          }

          case Disjunction(l, r) => {
            val (prefixedl, unprefixedl) = iter(l)
            val (prefixedr, unprefixedr) = iter(r)
            (optDisjuction(prefixedl, prefixedr), optDisjuction(unprefixedl, unprefixedr))
          }

          case Transform(fun, inv, inner) => {
            val (prefixed, unprefixed) = iter(inner)
            (prefixed.map(_.map(fun, inv)), unprefixed.map(_.map(fun, inv)))
          }

          case Marked(_, inner) => iter(inner)

          case Recursive(_, inner) => {
            if(enterRecursive){
              val (prefixed, unprefixed) = iter(inner)
              (prefixed.map(recursive(_)), unprefixed.map(recursive(_)))
            }
            else{
              (None, Some(syntax))
            }
          }
        }
      }
    }

    iter(syntax)
  }

  /**
    * Split a recursive syntax into its left recursive component
    * and its non-left recursive component.
    * 
    * @param syntax The syntax to split.
    * @return The left recursive component if any, and the component which isn't if any.
    *
    * @group transformation
    */
  def splitLeftRecursive[A](syntax: Recursive[A]): (Option[Syntax[A]], Option[Syntax[A]]) = {
    val (leftRec, nonLeftRec) = splitByPrefix(syntax.inner, syntax == _, false)
    (leftRec.map(recursive(_)), nonLeftRec.map(recursive(_)))
  }

}