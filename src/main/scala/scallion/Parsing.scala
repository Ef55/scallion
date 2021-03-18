/* Copyright 2020 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scallion

import scala.language.implicitConversions

import scala.annotation.tailrec

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import java.util.WeakHashMap

/** This trait implements LL(1) parsing with derivatives. */
trait Parsing extends SyntaxesProperties { self: Syntaxes =>

  /** Decorates syntaxes with methods for LL(1) properties.
    *
    * @group property
    */
  implicit def syntaxToLL1Properties[A](syntax: Syntax[A]): Properties[A] = {
    properties(syntax)
  }

  /** LL(1) parser.
    *
    * @group parsing
    */
  sealed trait Parser[A] { self =>

    /** The value, if any, corresponding to the empty sequence of tokens in `this` parser.
      *
      * @group property
      */
    def nullable: Option[A]

    /** Indicates if the empty sequence is described by `this` parser.
      *
      * @group property
      */
    def isNullable: Boolean = nullable.nonEmpty

    /** Indicates if there exists a finite sequence of tokens that `this` parser describes.
      *
      * @group property
      */
    def isProductive: Boolean = isNullable || first.nonEmpty

    /** Returns the set of token kinds that are accepted as the first token by `this` parser.
      *
      * @group property
      */
    def first: Set[Kind]

    /** Syntax corresponding to this parser.
      *
      * @group property
      */
    def syntax: Syntax[A]

    /** Returns the smallest interesting prefixes appropriately marked.
      *
      * @group property
      */
    def markedPrefixes(marks: Set[Mark]): Syntax[_]

    /** Parses a sequence of tokens.
      *
      * @group parsing
      */
    def apply(tokens: Iterator[Token]): ParseResult[A]

    /** Applies the given function on the result of the parser.
     *
     * @group parsing
     */
    def map[B](f: A => B): Parser[B] = {
      new Parser[B] {
        override def nullable = self.nullable.map(f)
        override def first = self.first
        override def syntax = self.syntax.map(f)
        override def markedPrefixes(marks: Set[Mark]): Syntax[_] =
          self.markedPrefixes(marks)

        override def apply(tokens: Iterator[Token]): ParseResult[B] = {
          self.apply(tokens).map(f)
        }
      }
    }
  }

  /** Result of parsing.
    *
    * @group result
    */
  sealed trait ParseResult[A] {

    /** Parser for the rest of input. */
    val rest: Parser[A]

    /** Returns the parsed value, if any. */
    def getValue: Option[A] = this match {
      case Parsed(value, _) => Some(value)
      case _ => None
    }

    /** Applies the given function on the parsed result. */
    def map[B](f: A => B): ParseResult[B] = this match {
      case Parsed(value, rest)          => Parsed(f(value), rest.map(f))
      case UnexpectedEnd(rest)          => UnexpectedEnd(rest.map(f))
      case UnexpectedToken(token, rest) => UnexpectedToken(token, rest.map(f))
    }
  }

  /** Indicates that the input has been fully parsed, resulting in a `value`.
    *
    * A parser for subsequent input is also provided.
    *
    * @param value The value produced.
    * @param rest  Parser for more input.
    *
    * @group result
    */
  case class Parsed[A](value: A, rest: Parser[A]) extends ParseResult[A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The parser at the point of error is returned.
    *
    * @param token The token at fault.
    * @param rest  Parser at the point of error.
    *
    * @group result
    */
  case class UnexpectedToken[A](token: Token, rest: Parser[A]) extends ParseResult[A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `syntax` for subsequent input is provided.
    *
    * @param syntax Syntax at the end of input.
    *
    * @group result
    */
  case class UnexpectedEnd[A](rest: Parser[A]) extends ParseResult[A]

  /** Factory of LL(1) parsers.
    *
    * @group parsing
    */
  object Parser {

    /** Builds a LL(1) parser from a syntax description.
      * In case the syntax is not LL(1),
      * returns the set of conflicts instead of a parser.
      *
      * @param syntax The description of the syntax.
      * @group parsing
      */
    def build[A](syntax: Syntax[A]): Either[Set[Conflict], Parser[A]] =
      scala.util.Try(apply(syntax, enforceLL1=true)) match {
        case scala.util.Success(parser) => Right(parser)
        case scala.util.Failure(ConflictException(conflicts)) => Left(conflicts)
        case scala.util.Failure(exception) => throw exception
      }

    /** Cache of transformation from syntax to LL(1) parser. */
    private val syntaxToTreeCache: WeakHashMap[Syntax[_], Tree[_]] = new WeakHashMap()

    /** Builds a LL(1) parser from a syntax description.
      *
      * @param syntax     The description of the syntax.
      * @param enforceLL1 Indicates if the method should throw a
      *                   `ConflictException` when the `syntax` is not LL(1).
      *                   `true` by default.
      * @throws ConflictException when the `syntax` is not LL(1) and `enforceLL1` is not set to `false`.
      * @group parsing
      */
    def apply[A](syntax: Syntax[A], enforceLL1: Boolean = true): Parser[A] = synchronized {

      // Handles caching.
      if (syntaxToTreeCache.containsKey(syntax)) {
        lazy val conflicts = properties(syntax).conflicts
        if (enforceLL1 && conflicts.nonEmpty) {
          throw ConflictException(conflicts)
        }
        val tree: Tree[A] = syntaxToTreeCache.get(syntax).asInstanceOf[Tree[A]]
        return Focused(tree, Empty())
      }

      // Cache miss... Real work begins.
      val recTrees: HashMap[RecId, Any] = new HashMap()

      def buildTree[A](syntaxCell: SyntaxCell[A]): Tree[A] = {
        val tree: Tree[A] = syntaxCell match {
          case SyntaxCell.Success(value, _) =>
            new Tree.Success(value) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Failure(_) =>
            new Tree.Failure[A]() {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Elem(kind, _) =>
            new Tree.Elem(kind) {
              override val nullable: Option[Token] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[Token] = syntaxCell.syntax
            }
          case SyntaxCell.Disjunction(left, right, _) =>
            new Tree.Disjunction[A](buildTree(left), buildTree(right)) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Sequence(left: SyntaxCell[tA], right: SyntaxCell[tB], _) =>
            new Tree.Sequence[tA, tB](buildTree(left), buildTree(right)) {
              override val nullable: Option[tA ~ tB] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[tA ~ tB] = syntaxCell.syntax
            }
          case SyntaxCell.Marked(inner, mark, _) =>
            new Tree.Marked[A](buildTree(inner), mark) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, _) =>
            new Tree.Transform[tA, A](buildTree(inner), function, inverse) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Recursive(recInner, recId, _) => recTrees.get(recId) match {
            case None => {
              val rec = new Tree.Recursive[A] {
                override val id = recId
                override lazy val inner: Tree[A] = syntaxToTreeCache.get(recInner.syntax).asInstanceOf[Tree[A]]
                override val nullable: Option[A] = syntaxCell.nullableCell.get
                override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
                override val syntax: Syntax[A] = syntaxCell.syntax
              }

              recTrees += recId -> rec

              buildTree(recInner)

              rec
            }
            case Some(rec) => rec.asInstanceOf[Tree[A]]
          }
        }

        syntaxToTreeCache.put(syntaxCell.syntax, tree)

        tree
      }

      val syntaxCell: SyntaxCell[A] = cell(syntax)
      val tree: Tree[A] = buildTree(syntaxCell)

      lazy val conflicts: Set[Conflict] = properties(syntaxCell.syntax).conflicts
      if (enforceLL1 && conflicts.nonEmpty) {
        throw ConflictException(conflicts)
      }

      Focused(tree, Empty())
    }

    private case class Focused[A, B](tree: Tree[B], context: Context[B, A]) extends Parser[A] {

      override def nullable: Option[A] = {

        @tailrec
        def go[C](focused: Focused[A, C]): Option[A] =
          focused.tree.nullable match {
            case Some(value) => focused.context.empty(value) match {
              case Some(casted) => Some(casted)
              case None => go(focused.context.plug(value))
            }
            case None => None
          }

        go(this)
      }

      override def first: Set[Kind] = {

        @tailrec
        def go[C](context: Context[C, A], acc: Set[Kind]): Set[Kind] = context match {
          case Empty() => acc
          case Layered(layer: Layer[tA, tB], tail) => layer.followTree match {
            case Some(tree) => {
              val unioned = acc union tree.first
              if (tree.isNullable) {
                go(tail, unioned)
              }
              else {
                unioned
              }
            }
            case None => go(tail, acc)
          }
        }

        if (tree.isNullable) {
          go(context, tree.first)
        }
        else {
          tree.first
        }
      }

      override def syntax: Syntax[A] = {

        @tailrec def go[B](context: Context[B, A], syntax: Syntax[B]): Syntax[A] = context match {
          case e: Empty[tA] => syntax
          case Layered(layer, tail) => go(tail, layer(syntax))
        }

        go(context, tree.syntax)
      }

      def markedPrefixes(marks: Set[Mark]): Syntax[_] = {

        @tailrec
        def go[B](prefix: Option[Syntax[B]], nullable: Option[B], context: Context[B, A], acc: Seq[Syntax[_]]): Seq[Syntax[_]] = context match {
          case Empty() => acc ++ prefix
          case Layered(layer, rest) =>
            if (layer.marks.exists(marks.contains(_))) {
              if (nullable.isEmpty) {
                acc ++ prefix.toSeq
              }
              else {
                go(None, nullable.flatMap(layer.nullable(_)), rest, acc ++ prefix.toSeq)
              }
            }
            else (layer.followTree, nullable) match {
              case (Some(tree), Some(value)) => {
                val (_, covereds, missing) = tree.prefixCovering(marks)

                val newNullable = for {
                  l <- nullable
                  r <- tree.nullable
                } yield l ~ r

                val combined = for {
                  l <- prefix
                  r <- missing
                } yield l ~ r

                val fromLeft = for {
                  l <- prefix
                  v <- tree.nullable
                } yield l ~ self.epsilon(v)

                val fromRight = for {
                  r <- missing
                } yield self.epsilon(value) ~ r

                val disjuncts = Seq(combined, fromLeft, fromRight).flatten

                val newPrefix = if (disjuncts.isEmpty) {
                  None
                } else {
                  Some(oneOf(disjuncts : _*))
                }

                val tail = rest.asInstanceOf[Context[B ~ layer.FollowType, A]]

                go[B ~ layer.FollowType](newPrefix, newNullable, tail, covereds ++ acc)
              }
              case _ => go(prefix.map(layer(_)), nullable.flatMap(layer.nullable(_)), rest, acc)
            }
        }

        val (_, covereds, missing) = tree.prefixCovering(marks)

        val syntaxes = go(missing, tree.nullable, context, covereds)

        oneOf(syntaxes.map(_.up[Any]).toSeq : _*)
      }

      override def apply(tokens: Iterator[Token]): ParseResult[A] = {
        var current: Focused[A, _] = this
        while (tokens.hasNext) {
          val token = tokens.next()
          val kind: Kind = getKind(token)

          current.locate(kind) match {
            case None => return UnexpectedToken(token, current)
            case Some(focused) => {
              current = focused.pierce(token, kind)
            }
          }
        }

        current.nullable match {
          case Some(value) => Parsed(value, current)
          case None => UnexpectedEnd(current)
        }
      }

      private def locate(kind: Kind): Option[Focused[A, _]] = {
        @tailrec
        def go[B](tree: Tree[B], context: Context[B, A]): Option[Focused[A, _]] = {
          if (tree.first.contains(kind)) Some(Focused(tree, context))
          else if (context.isEmpty) None
          else tree.nullable match {
            case Some(value) => {
              context.plug(value) match {
                case newFocused: Focused[tA, tB] => go(newFocused.tree, newFocused.context)
              }
            }
            case None => None
          }
        }

        go(tree, context)
      }

      private def pierce(token: Token, kind: Kind): Focused[A, _] = {
        tree.pierce(kind, context).plug(token)
      }
    }

    private sealed trait Tree[A] {
      val nullable: Option[A]
      @inline def isNullable: Boolean = nullable.nonEmpty
      @inline def isProductive: Boolean = isNullable || first.nonEmpty
      val first: HashSet[Kind]
      val syntax: Syntax[A]

      def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]])

      def pierce[B](kind: Kind, context: Context[A, B]): Context[Token, B] = {

        @tailrec
        def go[A, B](tree: Tree[A], context: Context[A, B]): Context[Token, B] = tree match {
          case Tree.Elem(_) => context
          case tree: Tree.Sequence[tA, tB] =>
            if (tree.left.first.contains(kind)) {
              go(tree.left, Layer.FollowBy[tA, tB](tree.right) +: context)
            }
            else {
              go(tree.right, Layer.PrependValue[tA, tB](tree.left.nullable.get) +: context)
            }
          case Tree.Disjunction(left, right) =>
            if (left.first.contains(kind)) {
              go(left, context)
            }
            else {
              go(right, context)
            }
          case Tree.Marked(inner, mark) =>
            go(inner, Layer.Marked[A](mark, false) +: context)
          case tree: Tree.Transform[tA, tB] =>
            go(tree.inner, Layer.ApplyFunction[tA, tB](tree.function, tree.inverse) +: context)
          case Tree.Recursive(_, tree) =>
            go(tree, context)
        }


        val rev = pierceCache.get(kind) match {
          case Some(rev) => rev
          case None => {
            val extra = go(this, Empty[A]())
            val rev = extra.reverse
            pierceCache(kind) = rev
            rev
          }
        }

        rev.toContext(context)
      }

      private val pierceCache: HashMap[Kind, RevContext[Token, A]] = new HashMap()
    }

    private object Tree {
      sealed abstract case class Success[A](value: A) extends Tree[A] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]]) =
          (true, Vector(), None)
      }
      sealed abstract case class Failure[A]() extends Tree[A] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]]) =
          (false, Vector(), None)
      }
      sealed abstract case class Elem(kind: Kind) extends Tree[Token] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[Token]]) =
          (false, Vector(), Some(syntax))
      }
      sealed abstract case class Sequence[A, B](left: Tree[A], right: Tree[B]) extends Tree[A ~ B] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A ~ B]]) = {
          val (changedLeft, coveredsLeft, missingLeft) = left.prefixCovering(marks)

          left.nullable match {
            case None =>
              (changedLeft, coveredsLeft, missingLeft.map(_ ~ right.syntax))
            case Some(v) => {
              val (changedRight, coveredsRight, missingRight) = right.prefixCovering(marks)

              val covereds = coveredsLeft ++ coveredsRight

              val missing: Option[Syntax[A ~ B]] = (missingLeft, missingRight) match {
                case (Some(sl), Some(sr)) => Some(sl ~ right.syntax | self.epsilon(v) ~ sr)
                case (Some(sl), _) => Some(sl ~ right.syntax)
                case (_, Some(sr)) => Some(self.epsilon(v) ~ sr)
                case _ => None
              }
              (changedLeft || changedRight, covereds, missing)
            }
          }
        }
      }
      sealed abstract case class Disjunction[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]]) = {
          val (changedLeft, coveredsLeft, missingLeft) = left.prefixCovering(marks)
          val (changedRight, coveredsRight, missingRight) = right.prefixCovering(marks)

          val covereds = coveredsLeft ++ coveredsRight

          val missing = (missingLeft, missingRight) match {
            case (Some(sl), Some(sr)) => Some(Syntax.Disjunction(sl, sr))
            case (Some(sl), _) => Some(sl)
            case (_, or) => or
          }

          (changedLeft || changedRight, covereds, missing)
        }
      }
      sealed abstract case class Marked[A](inner: Tree[A], mark: Mark) extends Tree[A] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]]) = {
          val (changed, covereds, missing) = inner.prefixCovering(marks)

          if (marks.contains(mark)) {
            (true, covereds ++ missing.toVector, None)
          }
          else if (!changed) {
            (false, covereds, missing.map(Syntax.Marked(mark, _)))
          }
          else {
            (true, covereds, missing)
          }
        }
      }
      sealed abstract case class Transform[A, B](
          inner: Tree[A], function: A => B, inverse: B => Seq[A]) extends Tree[B] {
        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[B]]) = {
          val (changed, covereds, missing) = inner.prefixCovering(marks)

          (changed, covereds, missing.map(Syntax.Transform(function, inverse, _)))
        }
      }

      sealed abstract class Recursive[A] extends Tree[A] {

        /** Inner parser for this recursive parser. */
        def inner: Tree[A]

        /** Unique identifier for this recursive parser. */
        val id: RecId

        /** Checks if `this` is equal to `other`.
          *
          * @group other
          */
        override def equals(other: Any): Boolean =
          if (!other.isInstanceOf[Recursive[_]]) {
            false
          }
          else {
            val that = other.asInstanceOf[Recursive[_]]
            this.id == that.id
          }

        /** Returns the hash of this object.
          *
          * @group other
          */
        override def hashCode(): Int = id

        override def prefixCovering(marks: Set[Mark]): (Boolean, Seq[Syntax[_]], Option[Syntax[A]]) =
          inner.prefixCovering(marks)
      }

      object Recursive {
        def unapply[A](that: Tree[A]): Option[(RecId, Tree[A])] = {
          if (that.isInstanceOf[Recursive[_]]) {
            val other = that.asInstanceOf[Recursive[A]]
            Some((other.id, other.inner))
          }
          else {
            None
          }
        }
      }

      def epsilon[A](value: A): Tree[A] = {
        new Success(value) {
          override val nullable: Option[A] = Some(value)
          override val first: HashSet[Kind] = HashSet()
          override val syntax: Syntax[A] = self.epsilon(value)
        }
      }
    }

    /** Reverse of a context.
      *
      * When a context has a pointer to the first element and the tail,
      * a reverse context has a pointer to the last element and the init.
      */
    private sealed trait RevContext[A, B] {

      def toContext[C](acc: Context[B, C]): Context[A, C] = {

        @tailrec
        def go[E](rev: RevContext[A, E], acc: Context[E, C]): Context[A, C] = rev match {
          case RevEmpty() => acc.asInstanceOf[Context[A, C]]
          case RevLayered(init, last) => go(init, Layered(last, acc))
        }

        go(this, acc)
      }
    }

    /** Empty reversed context. */
    private case class RevEmpty[A]() extends RevContext[A, A]

    /** Layer of extra context at the end of a reversed context. */
    private case class RevLayered[A, B, C](
        init: RevContext[A, B],
        last: Layer[B, C]) extends RevContext[A, C]

    /** Represents a context around a [[Tree]].
      *
      * It denotes a single point within a tree.
      *
      * The context may contain many layers, each
      * successive layer indicating which
      * combinator was used on top.
      *
      * The single focal point is always
      * situated on the left of the tree.
      * The can not be any other tree preceding it,
      * nor any alternatives to it.
      */
    private sealed trait Context[A, B] {

      def empty(value: A): Option[B]

      def plug(value: A): Focused[B, _] = {

        @tailrec
        def go[A, B](context: Context[A, B], value: A): Focused[B, _] =
          context match {
            case Empty() => Focused(Tree.epsilon(value), context)
            case Layered(head, tail) => head(value) match {
              case Left(newValue) => go(tail, newValue)
              case Right(LayeredTree(tree, layer)) => Focused(tree, layer +: tail)
            }
          }

        go(this, value)
      }

      /** Adds an extra layer of context. */
      def +:[C](that: Layer[C, A]): Context[C, B] =
        Layered(that, this)

      /** Returns true if the context is empty, false otherwise. */
      def isEmpty: Boolean

      /** Reverses this context. */
      def reverse: RevContext[A, B] = {

        @tailrec
        def go[E](ctx: Context[E, B], acc: RevContext[A, E]): RevContext[A, B] = ctx match {
          case e: Empty[tA] => acc
          case Layered(head, tail) => go(tail, RevLayered(acc, head))
        }

        go(this, RevEmpty())
      }
    }

    /** Indicates that there are no extra layers of context. */
    private case class Empty[A]() extends Context[A, A] {
      override def empty(value: A): Option[A] = Some(value)
      override def isEmpty = true
    }

    /** Layer of extra context on top on a context. */
    private case class Layered[A, B, C](
        head: Layer[A, B],
        tail: Context[B, C]) extends Context[A, C] {
      override def empty(value: A): Option[C] = None
      override def isEmpty = false
    }

    private case class LayeredTree[A, B](tree: Tree[A], layer: Layer[A, B])

    private sealed trait Layer[A, B] {
      type FollowType
      def apply(value: A): Either[B, LayeredTree[_, B]]
      def apply(syntax: Syntax[A]): Syntax[B]
      def marks: Option[Mark]
      def followTree: Option[Tree[FollowType]]
      def nullable(value: A): Option[B]
    }

    private object Layer {
      case class Marked[A](mark: Mark, complete: Boolean) extends Layer[A, A] {
        override type FollowType = Nothing
        override def apply(value: A): Either[A, LayeredTree[_, A]] =
          Left(value)
        override def apply(syntax: Syntax[A]): Syntax[A] =
          if (complete) Syntax.Marked(mark, syntax) else syntax
        override def marks: Option[Mark] = Some(mark)
        override def followTree: Option[Tree[Nothing]] = None
        override def nullable(value: A): Option[A] = Some(value)
      }

      case class ApplyFunction[A, B](function: A => B, inverse: B => Seq[A]) extends Layer[A, B] {
        override type FollowType = Nothing
        override def apply(value: A): Either[B, LayeredTree[_, B]] =
          Left(function(value))
        override def apply(syntax: Syntax[A]): Syntax[B] =
          syntax.map(function, inverse)
        override def marks: Option[Mark] = None
        override def followTree: Option[Tree[Nothing]] = None
        override def nullable(value: A): Option[B] = Some(function(value))
      }

      case class PrependValue[A, B](first: A) extends Layer[B, A ~ B] {
        override type FollowType = Nothing
        override def apply(second: B): Either[A ~ B, LayeredTree[_, A ~ B]] =
          Left(first ~ second)
        override def apply(syntax: Syntax[B]): Syntax[A ~ B] =
          self.epsilon(first) ~ syntax
        override def marks: Option[Mark] = None
        override def followTree: Option[Tree[Nothing]] = None
        override def nullable(value: B): Option[A ~ B] = Some(first ~ value)
      }

      case class FollowBy[A, B](second: Tree[B]) extends Layer[A, A ~ B] {
        override type FollowType = B
        override def apply(first: A): Either[A ~ B, LayeredTree[_, A ~ B]] =
          Right(LayeredTree(second, PrependValue(first)))
        override def apply(syntax: Syntax[A]): Syntax[A ~ B] =
          syntax ~ second.syntax
        override def marks: Option[Mark] = None
        override def followTree: Option[Tree[B]] = Some(second)
        override def nullable(value: A): Option[A ~ B] = second.nullable.map(value ~ _)
      }
    }
  }
}
