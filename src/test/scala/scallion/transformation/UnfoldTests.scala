package scallion
package transformation

import org.scalatest._

class UnfoldTests extends ParsersTestHelper with Unfold with BooleanSyntaxes {

  "Unfold disjunctions" should "unfold trivial example" in {
    val grammar = tru ~ (falz | tru) ~ falz
    val expected = (tru ~ falz ~ falz) | (tru ~ tru ~ falz)
    val unfolded = unfoldDisjunctions(grammar)

    assertResult(expected)(unfolded)
  }

  it should "not modify the syntax if there is no need to" in {
    val grammar = (tru ~ falz ~ falz) | (tru ~ tru ~ falz) | (tru ~ falz ~ tru)
    assert(grammar eq unfoldDisjunctions(grammar))
  }

  "Unfold leftmost recursive" should "unfold trivial example" in {
    val grammar = recursive(tru) ~ falz
    val expected = tru ~ falz
    val unfolded = unfoldLeftmostRecursives(grammar)

    assertResult(expected)(unfolded)
  }

  it should "unfold all leftmost alternatives" in {
    val grammar = recursive(tru) ~ falz | recursive(falz) ~ tru
    val expected = tru ~ falz | falz ~ tru
    val unfolded = unfoldLeftmostRecursives(grammar)

    assertResult(expected)(unfolded)
  }

  it should "unfold not unfold non-leftmost recursives" in {
    val grammar = recursive(tru) ~ falz | falz ~ recursive(tru)
    val expected = tru ~ falz | falz ~ recursive(tru)
    val unfolded = unfoldLeftmostRecursives(grammar)

    assertStructuralEquivalence(expected)(unfolded)
  }

  it should "unfold null-prefixed recursives" in {
    val grammar = opt(recursive(tru)) ~ recursive(falz)
    val expected = opt(tru) ~ falz
    val unfolded = unfoldLeftmostRecursives(grammar)

    assertStructuralEquivalence(expected)(unfolded)
  }
}