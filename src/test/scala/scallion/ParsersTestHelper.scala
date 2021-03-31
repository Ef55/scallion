package scallion

import scallion.properties.StructuralEquivalence
import org.scalatest._

trait ParsersTestHelper extends FlatSpec with Parsers with StructuralEquivalence {

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

  def assertParses[A](parser: Parser[A], input: Seq[Token]): A = {
    parser(input.iterator) match {
      case Parsed(r, _)   => r
      case _              => fail(s"Parsing failed on `${input}`")
    }
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

  def assertStructuralEquivalence[A](expected: Syntax[A])(actual: Syntax[A]): Syntax[A] = {
    assert(structurallyEquivalent(actual, expected))
    actual
  }

}