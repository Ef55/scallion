package scallion.factorization

import org.scalatest._
import scallion._
import scallion.factorization._

class SubstitutionTests extends FlatSpec with Parsers with Substitution {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)

  "Substitution" should "work on simple example" ignore {
    val transform = (t3: Token ~ Token ~ Token) => t3 match {
      case a ~ b ~ c  => a && b && c
    }
    val invTransform = (b: Boolean) => Seq()
    val grammar = ( (tru | falz) ~ tru ~ falz ).map(transform, invTransform)
    val transformed = substitute(grammar, tru, falz)
    val expected = ( (falz | falz) ~ falz ~ falz ).map(transform, invTransform)
    assertResult(grammar)(transformed)
  }

  "Safe substitution" should "correctly replace matching trees" in {
    val grammar = 
      ((tru | falz) ~ tru) ~ falz ~ tru.mark("marked!")
    val substituted1 = substituteSafe(grammar, falz, tru.mark("!"))
    val substituted2 = substituteSafe(grammar, (tru | falz) ~ tru, tru.mark("!") ~ falz.mark("!"))

    val expected1 = ((tru | tru.mark("!")) ~ tru) ~ tru.mark("!") ~ tru.mark("marked!")
    val expected2 = (tru.mark("!") ~ falz.mark("!")) ~ falz ~ tru.mark("marked!")

    assertResult(expected1)(substituted1)
    assertResult(expected2)(substituted2)
  }

  it should "not loop on infinite recursions" in {
    lazy val infiniteRec: Syntax[Boolean] = recursive(falz | infiniteRec)
    val substituted = substituteSafe(infiniteRec, tru, falz)
    assertResult(infiniteRec, "The grammar should be the same")(substituted)
    assertThrows[ConflictException](Parser(infiniteRec))
  }

  it should "not change anything if the original didn't contain the syntax to substitute" in {
    lazy val grammar: Syntax[Boolean] = recursive(
      tru |
      (epsilon(true) ~ failure[Boolean]).map{case a~b => a&&b}.mark("marked!")
    )
    val substituted = substituteSafe(grammar, falz, epsilon(false).mark("Oops!"))
    assertResult(grammar, "The grammar should be the same")(substituted)
  }
}