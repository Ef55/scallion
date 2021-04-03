package scallion
package factorization

import org.scalatest._

class SubstitutionTests extends ParsersTestHelper with Substitution with BooleanSyntaxes {

  "Substitution" should "correctly replace matching trees" in {
    val grammar = 
      ((tru | falz) ~ tru) ~ falz ~ tru.mark("marked!")
    val substituted1 = substitute(grammar, falz, tru.mark("!"))
    val substituted2 = substitute(grammar, (tru | falz) ~ tru, tru.mark("!") ~ falz.mark("!"))

    val expected1 = ((tru | tru.mark("!")) ~ tru) ~ tru.mark("!") ~ tru.mark("marked!")
    val expected2 = (tru.mark("!") ~ falz.mark("!")) ~ falz ~ tru.mark("marked!")

    assertResult(expected1)(substituted1)
    assertResult(expected2)(substituted2)
  }

  it should "not loop on infinite recursions" in {
    lazy val infiniteRec: Syntax[Boolean] = recursive(falz | infiniteRec)
    val substituted = substitute(infiniteRec, tru, falz)
    assertStructuralEquivalence(infiniteRec)(substituted)
    assertHasConflicts(infiniteRec)
  }

  // it should "not change anything if the original didn't contain the syntax to substitute" ignore {
  //   lazy val grammar: Syntax[Boolean] = recursive(
  //     tru |
  //     (epsilon(true) ~ failure[Boolean]).map{case a~b => a&&b}.mark("marked!")
  //   )
  //   val substituted = substitute(grammar, falz, epsilon(false).mark("Oops!"))
  //   assertResult(grammar, "The grammar should be the same")(substituted)
  // }

  it should "support recursive substitution" in {
    lazy val grammar: Syntax[Boolean] = recursive( tru | grammar )
    lazy val expected: Syntax[Boolean] = recursive( falz | expected )
    val substituted = substitute(grammar, tru, falz)
      
    assertStructuralEquivalence(expected)(substituted)
  }

  it should "support both substitution and elimination mode" in {
    lazy val grammar: Syntax[Boolean] = recursive( (epsilon(true) ~ grammar).map{ case a ~ b => a && b} )
    lazy val substit: Syntax[Boolean] = recursive( (epsilon(false) ~ grammar).map{ case a ~ b => a || b} )
    lazy val expected: Syntax[Boolean] = recursive( (epsilon(false) ~ expected).map{ case a ~ b => a || b} )

    val substituted = substitute(grammar, grammar, substit, false)
    val substitutedE = substitute(grammar, grammar, substit, true)
    val eliminated = eliminate(grammar, grammar, substit)

    assertResult(substit)(substituted)
    assertStructuralEquivalence(expected)(eliminated)
    assertStructuralEquivalence(eliminated)(substitutedE)
  }

  it should "work on mutually recusive syntaxes" in {
    lazy val s1: Syntax[Boolean] =
      recursive( (tru ~ s2).map{ case a~b => a&&b } )
    lazy val s2: Syntax[Boolean] =
      recursive( (tru ~ s1).map{ case a~b => a||b} )

    lazy val e1: Syntax[Boolean] =
      recursive( (falz ~ e2).map{ case a~b => a&&b } )
    lazy val e2: Syntax[Boolean] =
      recursive( (falz ~ e1).map{ case a~b => a||b} )

    val substituted = substitute(s1, tru, falz)
    assertStructuralEquivalence(e1)(substituted)
  }
}