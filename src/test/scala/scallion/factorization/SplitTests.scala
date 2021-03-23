package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._
import scallion.properties._

class SplitTests extends FlatSpec with Parsers with Split with StructuralEquivalence {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 && p._2

  "Split nullable" should "correctly split trivial example" in {
    val grammar = tru | eps
    val (nonNul, nul) = splitNullable(grammar)

    assertResult(Some(tru))(nonNul)
    assertResult(Some(eps))(nul)
  }

  it should "correctly split through the different constructs" in {
    val cstr = tru | eps
    val grammar = (cstr ~ cstr).map(comb)
    val expNonNul = (tru ~ tru | tru ~ eps | eps ~ tru).map(comb)
    val expNul = (eps ~ eps).map(comb)
    val (nonNul, nul) = splitNullable(grammar)

    assert(nonNul.isDefined)
    assert(nul.isDefined)

    assert(structurallyEquivalent(nonNul.get, expNonNul))
    assert(structurallyEquivalent(nul.get, expNul))
  }

  it should "return a None component if the syntax is not Nullable or is Null" in {
    val nullSyntax = (eps ~ eps).map(comb) | eps
    val notNullableSyntax = (tru ~ falz).map(comb) | falz

    val nullResult = splitNullable(nullSyntax)
    val notNullableResult = splitNullable(notNullableSyntax)

    assert(nullResult._1.isEmpty, nullResult._2.isDefined)
    assert(notNullableResult._1.isDefined, notNullableResult._2.isEmpty)

    assert(structurallyEquivalent(nullSyntax, nullResult._2.get))
    assert(structurallyEquivalent(notNullableSyntax, notNullableResult._1.get))
  }

  "Split left recursive" should "detect the left recursion" in {
    lazy val rec: Recursive[Boolean] = recursive( (rec ~ tru | falz ~ rec).map(comb) ).asInstanceOf[Recursive[Boolean]]
    lazy val expLeftRec: Syntax[Boolean] = recursive( (rec ~ tru).map(comb) )
    lazy val expNonLeftRec: Syntax[Boolean] = recursive( (falz ~ rec).map(comb) )
    val (leftRec, nonLeftRec) = splitLeftRecursive(rec)

    assert(leftRec.isDefined)
    assert(nonLeftRec.isDefined)

    assert(structurallyEquivalent(leftRec.get, expLeftRec))
    assert(structurallyEquivalent(nonLeftRec.get, expNonLeftRec))
  }
}