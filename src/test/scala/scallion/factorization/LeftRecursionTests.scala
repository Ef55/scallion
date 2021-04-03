package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._
import scallion.properties._

class LeftRecursionTests extends ParsersTestHelper with LeftRecursion {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t
  def getValue(k: Kind): Token = k

  val tru = elem(true)
  val falz = elem(false)
  val any = tru | falz
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 || p._2

  "Direct left recursion elimination" should "eliminate direct left recursion" in {
    lazy val grammar: Recursive[Boolean] = recursive( (grammar ~ any).map(comb) | eps ).asInstanceOf[Recursive[Boolean]]
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
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(comb) | eps }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(comb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false ~ true)*
    val factorized = eliminateLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(parser, grammar, getValue, 3000)
  }

  it should "eliminate (in)direct left recursion without changing the underlying language" ignore {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( (b ~ tru) | (a ~ tru) ).map(comb) | eps }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(comb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false | true)*
    val factorized = eliminateLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(parser, grammar, getValue, 1)
  }
}