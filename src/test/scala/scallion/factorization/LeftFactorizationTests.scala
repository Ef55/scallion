package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._

object Tokens {
  sealed trait Token{
    def toChar: Char = this match {
      case Letter(c)    => c
      case Number(c)    => c 
      case Separator(c) => c 
    }
  }
  case class Letter(char: Char) extends Token
  case class Number(num: Char) extends Token
  case class Separator(sep: Char) extends Token

  sealed trait Kind
  case object LetterKind extends Kind
  case object NumberKind extends Kind
  case object SeparatorKind extends Kind
}
import Tokens._

object Lexer {
  def apply(input: String): Seq[Token] = {
    input.map( chr =>
      if(chr.isLetter){
        Letter(chr)
      }
      else if(chr.isDigit){
        Number(chr)
      }
      else{
        Separator(chr)
      }
    )
  }
}

class LeftFactorizationTests extends ParsersTestHelper with LeftFactorization with Substitution {
  type Token = Tokens.Token
  type Kind = Tokens.Kind

  override def getKind(token: Token): Kind = token match {
    case Letter(_)      => LetterKind
    case Number(_)      => NumberKind
    case Separator(_)   => SeparatorKind
  }

  import Syntax._

  val letter = elem(LetterKind)
  val number = elem(NumberKind)
  val inumber = number.map(n => { val Number(i) = n; i.asDigit })
  val sep = elem(SeparatorKind)

  def testGrammar[R](grammar: Syntax[R], factorized: Syntax[R], inputs: Seq[(String, R)]) {
    assertHasConflicts(grammar)
    val lInputs = inputs.map( p => (Lexer(p._1), p._2) )
    val parser = assertIsLL1(factorized)
    assertParseResults(parser, lInputs)
  }

  "Left factorization" should "work on simple grammar" in {
    val prefix =
      (letter ~ letter).map(_ => 0) |
      (letter ~ number).map(_ => 1) |
      (number ~ number).map(_ => 2)
    val grammar = (prefix ~ sep).map{ case i ~ _ => i}
    val factorized = leftFactorize(LetterKind, grammar)
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
    val factorize = leftFactorize(NumberKind, grammar)
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
    val factorized = leftFactorize(LetterKind, grammar)
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
    val factorized = leftFactorize(LetterKind, grammar)
    val tests = Seq(
      ("aa", 1),
      ("a", 2)
    )
    
    testGrammar(grammar, factorized, tests)
  }
}
