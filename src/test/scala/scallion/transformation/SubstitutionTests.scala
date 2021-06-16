package scallion
package transformation

import org.scalatest._

class SubstitutionTests extends ParsersTestHelper with Substitution with BooleanSyntaxes {
  import Syntax._

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

    val substituted = substitute(grammar, grammar, substit)
    val eliminated = eliminate(grammar, grammar, substit)

    assertResult(substit)(substituted)
    assertStructuralEquivalence(expected)(eliminated)
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

  it should "not loop on missing recursive guard" ignore {
    lazy val s = (tru ~ rec).map(orComb)
    lazy val rec: Syntax[Boolean] = recursive(s)

    val result = eliminate(rec, rec, s)
    assertStructuralEquivalence(s)(result)
  }

  it should "allow to substitute a syntax with itself" in {
    lazy val s = (tru ~ rec).map(orComb)
    lazy val rec: Syntax[Boolean] = recursive(s)

    val result1 = eliminate(rec, s, s)
    val result2 = eliminate(s, rec, rec)
    assertStructuralEquivalence(rec)(result1)
    assertStructuralEquivalence(s)(result2)
  }

  "Substitution (multiple)" should "allow to substitute multiple syntaxes at once" in {
    val syntax = tru ~ any
    val substitutions: Map[Syntax[_], Syntax[_]] = Map(
      (tru, falz),
      (any, epsT)
    )
    val expected = falz ~ epsT

    val result = substitute(syntax, substitutions, false)
    assertResult(expected)(result)
  }

  it should "work even with mutually recursive syntaxes" in {
    lazy val s1: Syntax[Boolean] =
      recursive( (tru ~ s2).map(andComb) )
    lazy val s2: Syntax[Boolean] =
      recursive( (epsT ~ s1).map(orComb) )

    lazy val e1: Syntax[Boolean] =
      recursive( (falz ~ e2).map(andComb) )
    lazy val e2: Syntax[Boolean] =
      recursive( (epsF ~ e1).map(orComb) )

    val substitutions: Map[Syntax[_], Syntax[_]] = Map(
      (tru, falz),
      (epsT, epsF)
    )

    val substituted = substitute(s1, substitutions, false)
    assertStructuralEquivalence(e1)(substituted)
  }

  it should "behave as expected on indirect left recursives" in {
    lazy val a: Syntax[Boolean] = recursive{ aIn }
    lazy val aIn = (b ~ tru).map(orComb) | epsF
    lazy val b: Syntax[Boolean] = recursive{ bIn }
    lazy val bIn = (a ~ falz).map(orComb)
    val grammar = a

    val expected = eliminate(a, b, bIn)
    val result = eliminate(grammar, (b, bIn))

    assertStructuralEquivalence(expected)(result)
  }

  it should "behave as expected on cross left recursives" in {
    lazy val a: Syntax[Boolean] = 
      recursive{ ( (b ~ tru) | (a ~ tru) ).map(orComb) | epsF }
    lazy val b: Syntax[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb) }

    lazy val transformA: Syntax[Boolean] = 
      recursive{ ( (tru ~ b) | (a ~ tru) ).map(orComb) | epsF }
    lazy val transformB: Syntax[Boolean] = 
      recursive{ ( (epsT ~ a) | (tru ~ b) ).map(orComb) }

    lazy val expectedA: Syntax[Boolean] =
      recursive{ ((tru ~ expectedB) | (expectedA ~ tru) ).map(orComb) | epsF }
    lazy val expectedB: Syntax[Boolean] = 
      recursive{ ( (epsT ~ expectedA) | (tru ~ expectedB) ).map(orComb) }

    val substs = Map[Syntax[_], Syntax[_]]((a -> transformA), (b -> transformB))
    val result = eliminate(a, substs)

    assertStructuralEquivalence(expectedA)(result)
  }
}