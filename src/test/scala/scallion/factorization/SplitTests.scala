package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._
import scallion.properties._

class SplitTests extends ParsersTestHelper with Split with StructuralEquivalence {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 && p._2

  "(Try) split nullable" should "correctly split trivial example" in {
    val grammar = tru | eps
    val (nonNul, nul) = trySplitNullable(grammar)

    assertResult(Some(tru))(nonNul)
    assertResult(Some(eps))(nul)
  }

  it should "correctly split through the different constructs" in {
    val cstr = tru | eps
    val grammar = (cstr ~ cstr).map(comb)
    val expNonNul = (tru ~ tru | tru ~ eps | eps ~ tru).map(comb)
    val expNul = (eps ~ eps).map(comb)
    val (nonNul, nul) = trySplitNullable(grammar)

    assert(nonNul.isDefined)
    assert(nul.isDefined)

    assertStructuralEquivalence(expNonNul)(nonNul.get)
    assertStructuralEquivalence(expNul)(nul.get)
  }

  it should "return a None component if the syntax is not Nullable or is Null" in {
    val nullSyntax = (eps ~ eps).map(comb) | eps
    val notNullableSyntax = (tru ~ falz).map(comb) | falz

    val nullResult = trySplitNullable(nullSyntax)
    val notNullableResult = trySplitNullable(notNullableSyntax)

    assert(nullResult._1.isEmpty)
    assert(nullResult._2.isDefined)
    assert(notNullableResult._1.isDefined)
    assert(notNullableResult._2.isEmpty)

    assertStructuralEquivalence(nullSyntax)(nullResult._2.get)
    assertStructuralEquivalence(notNullableSyntax)(notNullableResult._1.get)
  }

  "Split left recursive" should "detect the left recursion" in {
    lazy val rec: Recursive[Boolean] = recursive( (rec ~ tru | falz ~ rec).map(comb) ).asInstanceOf[Recursive[Boolean]]
    lazy val expLeftRec: Syntax[Boolean] = recursive( (rec ~ tru).map(comb) )
    lazy val expNonLeftRec: Syntax[Boolean] = recursive( (falz ~ rec).map(comb) )
    val (leftRec, nonLeftRec) = splitLeftRecursive(rec)

    assert(leftRec.isDefined)
    assert(nonLeftRec.isDefined)

    assertStructuralEquivalence(expLeftRec)(leftRec.get)
    assertStructuralEquivalence(expNonLeftRec)(nonLeftRec.get)
  }
}