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

class LeftFactorizationTests extends FlatSpec with Parsers with LeftFactorization {

  type Token = Tokens.Token
  type Kind = Tokens.Kind

  import Implicits._

  override def getKind(token: Token): Kind = token match {
    case Letter(_)      => LetterKind
    case Number(_)      => NumberKind
    case Separator(_)   => SeparatorKind
  }

  import Syntax._

  val letter = elem(LetterKind)
  val number = elem(NumberKind)
  val sep = elem(SeparatorKind)

  // Trivial example
  {
    val conflicting =
      (letter ~ letter).map(_ => 0) |
      (letter ~ number).map(_ => 1) |
      (number ~ number).map(_ => 2)
    val grammar = (conflicting ~ sep).map{ case i ~ _ => i}
    val factorized = leftFactorize(LetterKind, grammar)

    "Trivial example" should "have conflicts" in {
      assertThrows[ConflictException](Parser(grammar))
    }

    it should "be LL1 once factorized" in {
      val parser = Parser(factorized)
    }

    it should "parse strings as expected" in {
      val parser = Parser(factorized)
      for(input <- Seq(
        ("ab ", 0),
        ("a1-", 1),
        ("11,", 2)
      )){
        parser(Lexer(input._1).iterator) match {
          case Parsed(r, _)   => 
            assertResult(input._2, s"on input `${input._1}`")(r)
          case _              => 
            fail(s"Parsing failed on `${input._1}`")
        }
      }
    }
  }

  // Repsep example (Not yet supported)
  {
    val grammar = 
      repsep(letter, sep).map(_.map(_.toChar).mkString("")) |
      (letter ~ number).map{ case l ~ n => s"${l.toChar}${n.toChar}"}
    val factorized = leftFactorize(LetterKind, grammar)

    "Repsep grammar" should "have conflicts" in {
      assertThrows[ConflictException](Parser(grammar))
    }

    it should "be LL1 once factorized" in {
      val parser = Parser(factorized)
    }

    it should "parse strings as expected" in {
      def mkTestCase(str: String): (String, String) = 
        (str, str.filter(_.isLetterOrDigit))

      val parser = Parser(factorized)
      for(str <- Seq(
        "a,b,c",
        "x y z",
        "a1",
        "b2"
      )){
        val (input, expected) = mkTestCase(str)
        parser(Lexer(input).iterator) match {
          case Parsed(r, _)   => 
            assertResult(expected, s"on input `${input}`")(r)
          case _              => 
            fail(s"Parsing failed on `${input}`")
        }
      }
    }

  }
}