package scallion
package transformation

import org.scalatest._

class LeftFactorizationTests extends ParsersTestHelper with LeftFactorization with Substitution with StringSyntaxes {
  def testGrammar[R](grammar: Syntax[R], factorized: Syntax[R], inputs: Seq[(String, R)]) {
    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    assertParseResults(parser, inputs, Lexer.apply(_))
  }

  "Left factorization" should "work on simple grammar" in {
    val prefix =
      (letter ~ letter).map(_ => 0) |
      (letter ~ number).map(_ => 1) |
      (number ~ number).map(_ => 2)
    val grammar = (prefix ~ sep).map{ case i ~ _ => i}
    val factorized = leftFactorizeKind(LetterKind, grammar)
    val tests = Seq(
      ("ab ", 0),
      ("a1-", 1),
      ("11,", 2)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  it should "work on recursive grammar" in {
    lazy val grammar: Syntax[Int] = recursive(
      epsilon(0) | 
      inumber | 
      (inumber ~ sep ~ grammar).map{ case i ~ _ ~ j => i + j }
    )
    val factorize = leftFactorizeKind(NumberKind, grammar)
    val factorized = eliminate(factorize, grammar, factorize)
    val tests = Seq(
      ("", 0),
      ("0,0,0", 0),
      ("1,2,3", 6),
      ("1,2,3,4,5,6,7,8,9", 45)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  it should "work on grammar using repsep" in {
    import Implicits._
    def mkTestCase(str: String): (String, String) = (str, str.filter(_.isLetterOrDigit))

    val grammar = 
        repsep(letter, sep).map(_.map(_.toChar).mkString("")) |
        (letter ~ number).map{ case l ~ n => s"${l.toChar}${n.toChar}"}
    val factorized = leftFactorizeKind(LetterKind, grammar)
    val tests = Seq(
      "a,b,c",
      "x y z",
      "a1",
      "b2"
    ).map(mkTestCase(_))
    
    testGrammar(grammar, factorized, tests)
  }

  it should "work with non-terminal left-factor" in {
    val prefix = (letter ~ letter ~ letter).map(_ => 3)
    val grammar =
      (prefix ~ letter).map{ case i ~ _ => i + 1} |
      (prefix ~ inumber).map{ case i ~ j => i + j}
    val factorized = leftFactorize(prefix, grammar)
    val tests = Seq(
      ("aaaa", 4),
      ("aaa3", 6),
      ("efgh", 4),
      ("efg9", 12)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  it should "work on null-prefixed syntaxes" in {
    val grammar = (epsilon(1) ~ letter ~ letter).map(_ => 1) | letter.map(_ => 2)
    val factorized = leftFactorizeKind(LetterKind, grammar)
    val tests = Seq(
      ("aa", 1),
      ("a", 2)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  it should "work on nullable-prefixed syntaxes" in {
    val grammar = ( (opt(letter) ~ letter) || number ).map{
      case Left(None ~ _)     => 1
      case Left(Some(_) ~ _)  => 2
      case Right(_)           => 3
    }
    val factorized = leftFactorizeKind(LetterKind, grammar)
    val tests = Seq(
      ("a", 1),
      ("aa", 2),
      ("1", 3)
    )
    
    testGrammar(grammar, factorized, tests)
  }

  "Left factor out" should "correctly remove the prefix" in {
    val grammar = letter ~ number | number ~ letter
    val (factorized, remains) = leftFactorOut(letter, grammar)

    assertStructuralEquivalence(number ~ letter)(remains)
    val parser = assertIsLL1(factorized)
    assertParses(parser, Seq("1", "2"), Lexer.apply(_))
  }

  it should "break recursives when asked to" in {
    val grammar = recursive( letter )
    val (factorized, remains) = leftFactorOut(letter, grammar, true)

    val expected: Syntax[Token => Token] = recursive( epsilon(()).map(_ => x => x) )

    assertStructuralEquivalence(expected)(factorized)
    assertResult(failure)(remains)
  }

  it should "not break recursives when asked not to" in {
    val grammar = recursive( letter )
    val (factorized, remains) = leftFactorOut(letter, grammar, false)

    assertResult(failure)(factorized)
    assertResult(grammar)(remains)
  }
}
