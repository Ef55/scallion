package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._
import scallion.properties._

class LeftRecursionTests extends FlatSpec with Parsers with LeftRecursion {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)
  val any = tru | falz
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 || p._2

  "Direct eft recursion elimination" should "be able to eliminate left recursion" in {
    lazy val grammar: Recursive[Boolean] = recursive( (grammar ~ any).map(comb) | eps ).asInstanceOf[Recursive[Boolean]]
    val factorized = eliminateDirectLeftRecursion(grammar)

    assertThrows[ConflictException](Parser(grammar))
    val parser = Parser(factorized)
    val inputs = Seq(
      (Seq(), false),
      (Seq(false, false), false),
      (Seq(true), true)
    )

    for(input <- inputs){
      parser(input._1.iterator) match {
        case Parsed(r, _)   => 
          assertResult(input._2, s"on input `${input._1}`")(r)
        case _              => 
          fail(s"Parsing failed on `${input._1}`")
      }
    }
  }
}