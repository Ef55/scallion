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

trait LeftFactorizationTest extends FlatSpec with Parsers with LeftFactorization with Substitution {
  type Token = Tokens.Token
  type Kind = Tokens.Kind

  override def getKind(token: Token): Kind = token match {
    case Letter(_)      => LetterKind
    case Number(_)      => NumberKind
    case Separator(_)   => SeparatorKind
  }

  import Syntax._

  def letter = elem(LetterKind)
  def number = elem(NumberKind)
  def inumber = number.map(n => { val Number(i) = n; i.asDigit })
  def sep = elem(SeparatorKind)

  type Result
  def grammar: Syntax[Result]
  def factorized: Syntax[Result]
  def name: String
  def tests: Seq[(String, Result)]

  name should "have conflicts" in {
    assertThrows[ConflictException](Parser(grammar))
  }

  it should "be LL1 once factorized" in {
    try{
      val parser = Parser(factorized)
    }
    catch{
      case ConflictException(conflicts) => {
        fail(debugString(factorized))
      }
    }
  }

  it should "parse strings as expected" in {
    val parser = Parser(factorized)
    for(input <- tests){
      parser(Lexer(input._1).iterator) match {
        case Parsed(r, _)   => 
          assertResult(input._2, s"on input `${input._1}`")(r)
        case _              => 
          fail(s"Parsing failed on `${input._1}`")
      }
    }
  }
}

class LeftFactorizationTestSimple extends LeftFactorizationTest {
  type Result = Int
  val prefix =
      (letter ~ letter).map(_ => 0) |
      (letter ~ number).map(_ => 1) |
      (number ~ number).map(_ => 2)
  def grammar = (prefix ~ sep).map{ case i ~ _ => i}
  def factorized = leftFactorize(LetterKind, grammar)
  def name = "Simple grammar"
  def tests = Seq(
    ("ab ", 0),
    ("a1-", 1),
    ("11,", 2)
  )
}

class LeftFactorizationTestRecursive extends LeftFactorizationTest {
  type Result = Int
  val gramma: Syntax[Int] = recursive(
    epsilon(0) | 
    inumber | 
    (inumber ~ sep ~ gramma).map{ case i ~ _ ~ j => i + j }
  )
  def grammar = gramma
  lazy val factorize = leftFactorize(NumberKind, grammar)
  def factorized = substitute(factorize, grammar, factorize)
  def name = "Recursive grammar"
  def tests = Seq(
    ("", 0),
    ("0,0,0", 0),
    ("1,2,3", 6),
    ("1,2,3,4,5,6,7,8,9", 45)
  )
}

class LeftFactorizationTestRepsep extends LeftFactorizationTest {
  import Implicits._

  type Result = String
  def grammar = 
      repsep(letter, sep).map(_.map(_.toChar).mkString("")) |
      (letter ~ number).map{ case l ~ n => s"${l.toChar}${n.toChar}"}
  def factorized = leftFactorize(LetterKind, grammar)
  def name = "Repsep grammar"
  def mkTestCase(str: String): (String, String) = (str, str.filter(_.isLetterOrDigit))
  def tests = Seq(
    "a,b,c",
    "x y z",
    "a1",
    "b2"
  ).map(mkTestCase(_))
}

class LeftFactorizationTestNonTerminal extends LeftFactorizationTest {
  type Result = Int
  val prefix = (letter ~ letter ~ letter).map(_ => 3)
  def grammar =
    (prefix ~ letter).map{ case i ~ _ => i + 1} |
    (prefix ~ inumber).map{ case i ~ j => i + j}
  def factorized = leftFactorize(prefix, grammar)
  def name = "Non-terminal factorization"
  def tests = Seq(
    ("aaaa", 4),
    ("aaa3", 6),
    ("efgh", 4),
    ("efg9", 12)
  )
}