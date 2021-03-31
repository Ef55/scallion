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

  val tru = elem(true)
  val falz = elem(false)
  val any = tru | falz
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 || p._2

  "Direct left recursion elimination" should "be able to eliminate left recursion" in {
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
}