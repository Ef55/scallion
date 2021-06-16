package scallion
package transformation

import org.scalatest._

class TransformationsTests extends ParsersTestHelper with Transformations with BooleanSyntaxes {

  "Solve conflicts" should "work on simple first/first example" in {
    val grammar = (tru ~ falz) | (tru ~ tru)
    val solved = solveConflicts(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(solved)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "work on simple first/follow example" in {
    val grammar = (tru | epsF)  ~ tru
    val solved = solveConflicts(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(solved)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "work on first/follow example" in {
    val grammar = (falz ~ (tru | epsT)) ~ tru
    val solved = solveConflicts(grammar)

    outputGraph(grammar, "original")
    outputGraph(solved, "solved")

    assertHasConflicts(grammar)
    val parser = assertIsLL1(solved)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "work on simple left recursive example" in {
    lazy val grammar: Syntax[Boolean] = recursive {
      (grammar ~ tru).map(orComb) | epsT
    }
    val solved = solveConflicts(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(solved)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "work on `not immediately left recursise` syntaxes" in {
    lazy val rec: Syntax[Boolean] = recursive {
      (tru ~ rec).map(orComb) | epsT
    }
    val grammar = rec | (tru ~ falz).map(orComb)
    val solved = solveConflicts(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(solved)

    assertParseResult(true)(parser, List(true, false))
    assertParseResult(true)(parser, List(true))
    assertParseResult(true)(parser, List(true, true, true))
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

}