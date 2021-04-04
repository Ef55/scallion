package scallion

import scallion.properties.StructuralEquivalence
import org.scalatest._

trait ParsersTestHelper extends FlatSpec with Parsers with Enumeration with StructuralEquivalence {

  def assertHasConflicts(syntax: Syntax[_]): Unit = {
    assertThrows[ConflictException](Parser(syntax))
  }

  def assertIsLL1[A](syntax: Syntax[A]): Parser[A] = {
    try{
      Parser(syntax)
    }
    catch{
      case ConflictException(conflicts) => {
        fail(debugString(syntax))
      }
    }
  }

  def assertParses[A](parser: Parser[A], input: Iterator[Token]): A = {
    parser(input) match {
      case Parsed(r, _)   => r
      case _              => fail(s"Parsing failed on `${input}`")
    }
  }

  def assertParses[A](parser: Parser[A], input: Seq[Token]): A = assertParses(parser, input.iterator)

  def assertParses[A](parser: Parser[A], generator: Syntax[A], valuer: Kind => Token, count: Int): Seq[A] = {
    // toBuffer required to avoid lazy evaluation (which would result in nothing being tested)
    Enumerator(generator).take(count).map(input => assertParses(parser, input.map(valuer))).toBuffer
  }

  def assertParseResult[A](expected: A)(parser: Parser[A], input: Seq[Token]): A = {
    val result = assertParses(parser, input)
    assertResult(expected, s"on input `${input}`")(result)
    result
  }

  def assertParseResults[A](parser: Parser[A], inputsExpected: Seq[(Seq[Token], A)]): Seq[A] = {
    inputsExpected.map{
      case (input, expected) => assertParseResult(expected)(parser, input)
    }
  }

  def assertStructuralEquivalence[A](expected: Syntax[_])(actual: Syntax[A]): Syntax[A] = {
    assert(structurallyEquivalent(actual, expected))
    actual
  }

}