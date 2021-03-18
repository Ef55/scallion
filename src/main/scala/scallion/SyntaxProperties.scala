package scallion

import scallion.util.internal._

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.WeakHashMap

trait SyntaxesProperties { self: Syntaxes =>
  /** Cache of computation of LL(1) properties for syntaxes. */
  private val syntaxToCellCache: WeakHashMap[Syntax[_], SyntaxCell[_]] = new WeakHashMap()

  /** Follow-last set tagged with its source. */
  protected case class ShouldNotFollowEntry(source: Syntax.Disjunction[_], kinds: Set[Kind])
  
  /** Describes a LL(1) conflict.
    *
    * @group conflict
    */
  sealed trait Conflict {

    /** Source of the conflict. */
    val source: Syntax.Disjunction[_]
  }

  /** Contains the description of the various LL(1) conflicts.
    *
    * @group conflict
    */
  object Conflict {

    import Syntax._

    /** Indicates that both branches of a disjunction are nullable.
      *
      * @param source The source of the conflict.
      */
    case class NullableConflict(source: Disjunction[_]) extends Conflict

    /** Indicates that two branches of a disjunction share some same first token kinds.
      *
      * @param source      The source of the conflict.
      * @param ambiguities The conflicting kinds.
      */
    case class FirstConflict(source: Disjunction[_],
                             ambiguities: Set[Kind]) extends Conflict

    /** Indicates that an ambiguity arises due to a disjunction appearing somewhere in
      * the left-hand side of a sequence, that conflicts with the right-hand side of
      * that sequence.
      *
      * @param source      The source of the conflict.
      * @param root        The sequence in which the conflict occured.
      * @param ambiguities The conflicting kinds.
      */
    case class FollowConflict(source: Disjunction[_],
                              root: Sequence[_, _],
                              ambiguities: Set[Kind]) extends Conflict
  }
  import Conflict._

  /** Indicates that a syntax is not LL(1) due to various conflicts.
    *
    * @group conflict
    */
  case class ConflictException(conflicts: Set[Conflict]) extends Exception("Syntax is not LL(1).")

  /** Contains properties of syntaxes.
    *
    * @param nullable        A value associated to the empty string, if any.
    * @param first           The set of token kinds that can start valid sequences.
    * @param shouldNotFollow The set of token kinds that should not follow in sequence.
    * @param conflicts       The set of LL(1) conflicts of the syntax.
    *
    * @group property
    */
  case class Properties[A](
      nullable: Option[A],
      first: Set[Kind],
      shouldNotFollow: Set[Kind],
      conflicts: Set[Conflict]) {

    /** Indicates if the syntax accepts the empty sequence. */
    def isNullable: Boolean = nullable.nonEmpty

    /** Indicates if the syntax accepts at least one sequence. */
    def isProductive: Boolean = isNullable || first.nonEmpty

    /** Indicates if the syntax is LL(1). */
    def isLL1: Boolean = conflicts.isEmpty
  }

  protected sealed trait SyntaxCell[A] {
    def init(): Unit

    val syntax: Syntax[A]
    val productiveCell: Cell[Unit, Unit, Boolean] = new BooleanCell
    val nullableCell: Cell[A, A, Option[A]] = new OptionCell[A]
    val firstCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
    val snfCell: Cell[Set[ShouldNotFollowEntry],
      Set[ShouldNotFollowEntry], Set[ShouldNotFollowEntry]] = new SetCell[ShouldNotFollowEntry]
    val conflictCell: Cell[Set[Conflict], Set[Conflict], Set[Conflict]] = new SetCell[Conflict]
  }

  protected object SyntaxCell {

    case class Success[A](value: A, syntax: Syntax[A]) extends SyntaxCell[A] {
      override def init(): Unit = {
        productiveCell(())
        nullableCell(value)
      }
    }

    case class Failure[A](syntax: Syntax[A]) extends SyntaxCell[A] {
      override def init(): Unit = ()
    }

    case class Elem(kind: Kind, syntax: Syntax[Token]) extends SyntaxCell[Token] {
      override def init(): Unit = {
        productiveCell(())
        firstCell(Set(kind))
      }
    }

    case class Disjunction[A](left: SyntaxCell[A], right: SyntaxCell[A], syntax: Syntax[A])
        extends SyntaxCell[A] {
      override def init(): Unit = {

        left.init()
        right.init()

        // Productivity
        left.productiveCell.register(productiveCell)
        right.productiveCell.register(productiveCell)

        // Nullability
        left.nullableCell.register(nullableCell)
        right.nullableCell.register(nullableCell)

        // First
        left.firstCell.register(firstCell)
        right.firstCell.register(firstCell)

        // Should not follow
        left.snfCell.register(snfCell)
        right.snfCell.register(snfCell)

        val snfLeft: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
          new GatedCell[Set[ShouldNotFollowEntry]]

        left.firstCell.register(snfLeft.contramap(ks =>
          Some(Set(ShouldNotFollowEntry(syntax.asInstanceOf[Syntax.Disjunction[_]], ks)))))
        right.nullableCell.register(snfLeft.contramap((_: A) => None))
        snfLeft.register(snfCell)

        val snfRight: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
          new GatedCell[Set[ShouldNotFollowEntry]]

        left.nullableCell.register(snfRight.contramap((_: A) => None))
        right.firstCell.register(snfRight.contramap(ks =>
          Some(Set(ShouldNotFollowEntry(syntax.asInstanceOf[Syntax.Disjunction[_]], ks)))))
        snfRight.register(snfCell)

        left.conflictCell.register(conflictCell)
        right.conflictCell.register(conflictCell)
      }
    }

    case class Sequence[A, B](left: SyntaxCell[A], right: SyntaxCell[B], syntax: Syntax[A ~ B])
        extends SyntaxCell[A ~ B] {

      override def init(): Unit = {

        left.init()
        right.init()

        // Productivity
        val mergeProductive: Cell[Either[Unit, Unit], Unit, Option[Unit]] =
          new MergeOnceCell[Unit, Unit, Unit]((_: Unit, _: Unit) => ())

        left.productiveCell.register(mergeProductive.contramap(Left(_)))
        right.productiveCell.register(mergeProductive.contramap(Right(_)))
        mergeProductive.register(productiveCell)

        // Nullability
        val mergeNullable: Cell[Either[A, B], (A ~ B), Option[A ~ B]] =
          new MergeOnceCell[A, B, A ~ B]((leftValue: A, rightValue: B) => leftValue ~ rightValue)

        left.nullableCell.register(mergeNullable.contramap(Left(_)))
        right.nullableCell.register(mergeNullable.contramap(Right(_)))
        mergeNullable.register(nullableCell)

        // First
        val firstLeft: Cell[Option[Set[Kind]], Set[Kind], Any] =
          new GatedCell[Set[Kind]]

        left.firstCell.register(firstLeft.contramap(Some(_)))
        right.productiveCell.register(firstLeft.contramap((_: Unit) => None))
        firstLeft.register(firstCell)

        val firstRight: Cell[Option[Set[Kind]], Set[Kind], Any] =
          new GatedCell[Set[Kind]]

        left.nullableCell.register(firstRight.contramap((_: A) => None))
        right.firstCell.register(firstRight.contramap(Some(_)))
        firstRight.register(firstCell)

        // Should not follow
        val snfLeft: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
          new GatedCell[Set[ShouldNotFollowEntry]]

        left.snfCell.register(snfLeft.contramap(Some(_)))
        right.nullableCell.register(snfLeft.contramap((_: B) => None))
        snfLeft.register(snfCell)

        val snfRight: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
          new GatedCell[Set[ShouldNotFollowEntry]]

        left.productiveCell.register(snfRight.contramap((_: Unit) => None))
        right.snfCell.register(snfRight.contramap(Some(_)))
        snfRight.register(snfCell)

        left.conflictCell.register(conflictCell)
        right.conflictCell.register(conflictCell)
      }
    }

    case class Marked[A](
        inner: SyntaxCell[A],
        mark: Mark,
        syntax: Syntax[A]) extends SyntaxCell[A] {

      override def init(): Unit = {
        inner.init()

        inner.productiveCell.register(productiveCell)
        inner.nullableCell.register(nullableCell)
        inner.firstCell.register(firstCell)
        inner.snfCell.register(snfCell)
        inner.conflictCell.register(conflictCell)
      }
    }

    case class Transform[A, B](
        inner: SyntaxCell[A],
        function: A => B,
        inverse: B => Seq[A],
        syntax: Syntax[B]) extends SyntaxCell[B] {

      override def init(): Unit = {
        inner.init()

        inner.productiveCell.register(productiveCell)
        inner.nullableCell.register(nullableCell.contramap(function))
        inner.firstCell.register(firstCell)
        inner.snfCell.register(snfCell)
        inner.conflictCell.register(conflictCell)
      }
    }

    abstract class Recursive[A] extends SyntaxCell[A] {
      def inner: SyntaxCell[A]
      val id: RecId

      var inited: Boolean = false

      override def init(): Unit = {
        if (!inited) {
          inited = true

          inner.init()

          inner.productiveCell.register(productiveCell)
          inner.nullableCell.register(nullableCell)
          inner.firstCell.register(firstCell)
          inner.snfCell.register(snfCell)
          inner.conflictCell.register(conflictCell)
        }
      }
    }

    object Recursive {
      def apply[A](cell: => SyntaxCell[A], recId: RecId, syn: Syntax[A]): SyntaxCell[A] =
        new Recursive[A] {
          override val id: RecId = recId
          override lazy val inner: SyntaxCell[A] = cell
          override val syntax: Syntax[A] = syn
        }

      def unapply[A](that: SyntaxCell[A]): Option[(SyntaxCell[A], RecId, Syntax[A])] = {
        if (that.isInstanceOf[Recursive[_]]) {
          val other = that.asInstanceOf[Recursive[A]]
          Some((other.inner, other.id, other.syntax))
        }
        else {
          None
        }
      }
    }
  }
  import SyntaxCell._

  protected def cell[A](syntax: Syntax[A]): SyntaxCell[A] = {
    syntaxToCellCache
      .getOrElse(syntax, syntaxToCell(syntax))
      .asInstanceOf[SyntaxCell[A]]
  }

  protected def properties[A](syntax: Syntax[A]): Properties[A] = {
    val syntaxCell = cell(syntax)
    Properties(
        syntaxCell.nullableCell.get,
        syntaxCell.firstCell.get,
        syntaxCell.snfCell.get.flatMap(_.kinds),
        syntaxCell.conflictCell.get
    )
  }

  private def syntaxToCell[A](syntax: Syntax[A]): SyntaxCell[A] = {

    val recCells: HashMap[RecId, SyntaxCell[_]] = new HashMap()
    def buildCell[A](syntax: Syntax[A]): SyntaxCell[A] = {
      syntaxToCellCache.getOrElseUpdate( 
        syntax,
        syntax match {
          case Syntax.Success(value) => SyntaxCell.Success(value, syntax)
          case Syntax.Failure() => SyntaxCell.Failure(syntax)
          case Syntax.Elem(kind) => SyntaxCell.Elem(kind, syntax)
          case Syntax.Disjunction(left, right) =>
            SyntaxCell.Disjunction(buildCell(left), buildCell(right), syntax)
          case Syntax.Sequence(left, right) =>
            SyntaxCell.Sequence(buildCell(left), buildCell(right), syntax)
          case Syntax.Marked(mark, inner) =>
            SyntaxCell.Marked(buildCell(inner), mark, syntax)
          case Syntax.Transform(function, inverse, inner) =>
            SyntaxCell.Transform(buildCell(inner), function, inverse, syntax)
          case Syntax.Recursive(id, inner) => recCells.get(id) match {
            case None => {
              val rec = SyntaxCell.Recursive(buildCell(inner), id, syntax)
              recCells += id -> rec
              rec
            }
            case Some(rec) => rec.asInstanceOf[SyntaxCell.Recursive[A]]
          }
        }
      ).asInstanceOf[SyntaxCell[A]]
    }

    var recChecked: Set[RecId] = Set()
    def checkConflicts[A](syntaxCell: SyntaxCell[A]): Unit = syntaxCell match {
      case SyntaxCell.Success(value, _) => ()
      case SyntaxCell.Failure(_) => ()
      case SyntaxCell.Elem(kind, _) => ()
      case SyntaxCell.Disjunction(left, right, syntax) => {
        checkConflicts(left)
        checkConflicts(right)
        if (left.nullableCell.get.nonEmpty && right.nullableCell.get.nonEmpty) {
          syntaxCell.conflictCell(Set(NullableConflict(
            syntaxCell.syntax.asInstanceOf[Syntax.Disjunction[_]])))
        }
        val intersecting = left.firstCell.get.intersect(right.firstCell.get)
        if (intersecting.nonEmpty) {
          syntaxCell.conflictCell(Set(FirstConflict(
            syntaxCell.syntax.asInstanceOf[Syntax.Disjunction[_]], intersecting)))
        }
      }
      case SyntaxCell.Sequence(left: SyntaxCell[tA], right: SyntaxCell[tB], syntax) => {
        checkConflicts(left)
        checkConflicts(right)
        val firstSet: Set[Kind] = right.firstCell.get
        val snfEntries: Set[ShouldNotFollowEntry] = left.snfCell.get
        for (entry <- snfEntries) {
          val ambiguities = entry.kinds.intersect(firstSet)
          if (ambiguities.nonEmpty) {
            syntaxCell.conflictCell(Set(FollowConflict(
              entry.source, syntaxCell.syntax.asInstanceOf[Syntax.Sequence[_, _]], ambiguities)))
          }
        }
      }
      case SyntaxCell.Marked(inner: SyntaxCell[tA], mark, syntax) => {
        checkConflicts(inner)
      }
      case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, syntax) => {
        checkConflicts(inner)
      }
      case SyntaxCell.Recursive(recInner, recId, _) => if (!recChecked.contains(recId)) {
        recChecked += recId
        checkConflicts(recInner)
      }
    }

    val cell = buildCell(syntax)
    cell.init()
    checkConflicts(cell)
    cell
  }
}
