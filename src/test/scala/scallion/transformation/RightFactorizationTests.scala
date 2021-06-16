package scallion
package transformation

import org.scalatest._

class RightFactorizationTests extends ParsersTestHelper with RightFactorization with Substitution with StringSyntaxes {
  def testGrammar[R](grammar: Syntax[R], factorized: Syntax[R], inputs: Seq[(String, R)]) {
    val parser = assertIsLL1(factorized)
    assertParseResults(parser, inputs, Lexer.apply(_))
  }

  "Right factorization" should "work on simple grammar" in {
    val prefix =
      (letter ~ number).map(_ => 1) |
      (number ~ number).map(_ => 2)
    val grammar = prefix
    val factorized = rightFactorizeKind(NumberKind, grammar)
    val tests = Seq(
      ("a1", 1),
      ("11", 2)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  "Right factor out" should "correctly remove the suffix" in {
    val grammar = letter ~ number | number ~ letter
    val (factorized, remains) = rightFactorOut(number, grammar)

    assertStructuralEquivalence(number ~ letter)(remains)
    val parser = assertIsLL1(factorized)
    assertParses(parser, Seq("a", "b"), Lexer.apply(_))
  }

  it should "break recursives when asked to" in {
    val grammar = recursive( letter )
    val (factorized, remains) = rightFactorOut(letter, grammar, true)

    val expected: Syntax[Token => Token] = recursive( epsilon(()).map(_ => x => x) )

    assertStructuralEquivalence(expected)(factorized)
    assertResult(failure)(remains)
  }

  it should "not break recursives when asked not to" in {
    val grammar = recursive( letter )
    val (factorized, remains) = rightFactorOut(letter, grammar, false)

    assertResult(failure)(factorized)
    assertResult(grammar)(remains)
  }
}
