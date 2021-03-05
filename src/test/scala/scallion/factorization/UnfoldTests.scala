package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._

class UnfoldTests extends FlatSpec with Parsers with Unfold {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)

  "Unfold disjunctions" should "correctly unfold trivial example" in {
    val grammar = tru ~ (falz | tru) ~ falz
    val expected = (tru ~ falz ~ falz) | (tru ~ tru ~ falz)
    val unfolded = unfoldDisjunctions(grammar)

    assertResult(expected)(unfolded)
  }

  it should "not modify the syntax if there is no need to" in {
    val grammar = (tru ~ falz ~ falz) | (tru ~ tru ~ falz) | (tru ~ falz ~ tru)
    assert(grammar eq unfoldDisjunctions(grammar))
  }
}