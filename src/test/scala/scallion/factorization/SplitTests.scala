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

  "Split nullable" should "correctly split trivial example" in {
    val grammar = tru | eps
    val (nonNul, nul) = splitNullable(grammar)

    assertResult(Some(tru))(nonNul)
    assertResult(Some(eps))(nul)
  }

  "Split nullable" should "correctly split through the different constructs" in {
    val fun = (p: Boolean ~ Boolean) => p._1 && p._2
    val cstr = tru | eps
    val grammar = (cstr ~ cstr).map(fun)
    val expNonNul = (tru ~ tru | tru ~ eps | eps ~ tru).map(fun)
    val expNul = (eps ~ eps).map(fun)
    val (nonNul, nul) = splitNullable(grammar)

    assert(nonNul.isDefined)
    assert(nul.isDefined)

    assert(structurallyEquivalent(nonNul.get, expNonNul))
    assert(structurallyEquivalent(nul.get, expNul))
  }
}