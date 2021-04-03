package scallion
package factorization

import org.scalatest._

class LeftRecursionTests extends ParsersTestHelper with LeftRecursion with BooleanSyntaxes {
  import Syntax.Recursive

  "Direct left recursion elimination" should "eliminate direct left recursion" in {
    lazy val grammar: Recursive[Boolean] = recursive( (grammar ~ any).map(orComb) | epsF ).asInstanceOf[Recursive[Boolean]]
    val factorized = eliminateDirectLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    val inputs = Seq(
      (Seq(), false),
      (Seq(false, false), false),
      (Seq(true), true)
    )

    assertParseResults(parser, inputs)
  }

  "Left recursion elimination" should "eliminate indirect left recursion without changing the underlying language" in {
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(orComb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false ~ true)*
    val factorized = eliminateLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(parser, grammar, getValue, 3000)
  }

  it should "eliminate (in)direct left recursion without changing the underlying language" ignore {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( (b ~ tru) | (a ~ tru) ).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false | true)*
    val factorized = eliminateLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(parser, grammar, getValue, 1)
  }
}