package scallion
package transformation

import org.scalatest._

class SplitTests extends ParsersTestHelper with Split with BooleanSyntaxes {
  import Syntax.Recursive

  "(Try) split nullable" should "correctly split trivial example" in {
    val grammar = tru | epsF
    val (nonNul, nul) = trySplitNullable(grammar)

    assertResult(Some(tru))(nonNul)
    assertResult(Some(epsF))(nul)
  }

  it should "correctly split through the different constructs" in {
    val cstr = tru | epsF
    val grammar = (cstr ~ cstr).map(andComb)
    val expNonNul = (tru ~ tru | tru ~ epsF | epsF ~ tru).map(andComb)
    val expNul = (epsF ~ epsF).map(andComb)
    val (nonNul, nul) = trySplitNullable(grammar)

    assert(nonNul.isDefined)
    assert(nul.isDefined)

    assertStructuralEquivalence(expNonNul)(nonNul.get)
    assertStructuralEquivalence(expNul)(nul.get)
  }

  it should "return a None component if the syntax is not Nullable or is Null" in {
    val nullSyntax = (epsF ~ epsF).map(andComb) | epsF
    val notNullableSyntax = (tru ~ falz).map(andComb) | falz

    val nullResult = trySplitNullable(nullSyntax)
    val notNullableResult = trySplitNullable(notNullableSyntax)

    assert(nullResult._1.isEmpty)
    assert(nullResult._2.isDefined)
    assert(notNullableResult._1.isDefined)
    assert(notNullableResult._2.isEmpty)

    assertStructuralEquivalence(nullSyntax)(nullResult._2.get)
    assertStructuralEquivalence(notNullableSyntax)(notNullableResult._1.get)
  }

  "Split left recursive" should "detect the left recursion" in {
    lazy val rec: Recursive[Boolean] = recursive( (rec ~ tru | falz ~ rec).map(andComb) ).asInstanceOf[Recursive[Boolean]]
    lazy val expLeftRec: Syntax[Boolean] = recursive( (rec ~ tru).map(andComb) )
    lazy val expNonLeftRec: Syntax[Boolean] = recursive( (falz ~ rec).map(andComb) )
    val (leftRec, nonLeftRec) = splitLeftRecursive(rec)

    assert(leftRec.isDefined)
    assert(nonLeftRec.isDefined)

    assertStructuralEquivalence(expLeftRec)(leftRec.get)
    assertStructuralEquivalence(expNonLeftRec)(nonLeftRec.get)
  }

  it should "not loop if some prefix is left recursive" in {
    lazy val rec1: Recursive[Boolean] = recursive( (rec1 ~ tru).map(andComb) | rec2 ).asInstanceOf[Recursive[Boolean]]
    lazy val rec2: Syntax[Boolean] = recursive( rec2 )
    val (leftRec, nonLeftRec) = splitLeftRecursive(rec1)

    assert(leftRec.isDefined)
    assert(nonLeftRec.isDefined)

    assertStructuralEquivalence( recursive( (rec1 ~ tru).map(andComb) ) )(leftRec.get)
    assertStructuralEquivalence(rec2)(nonLeftRec.get)
  }

  it should "behaves correctly on mutual left recursions" in {
    lazy val rec1: Recursive[Boolean] = 
      recursive( rec2 | tru ).asInstanceOf[Recursive[Boolean]]
    lazy val rec2: Recursive[Boolean] = 
      recursive( (rec1 ~ falz).map(andComb) | tru ).asInstanceOf[Recursive[Boolean]]
    val (leftRec, nonLeftRec) = splitLeftRecursive(rec1)

    assert(leftRec.isEmpty)
    assert(nonLeftRec.isDefined)

    assertStructuralEquivalence(rec1)(nonLeftRec.get)
  }
}