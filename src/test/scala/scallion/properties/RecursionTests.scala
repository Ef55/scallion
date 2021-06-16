package scallion.properties

import scala.collection.immutable.Set
import org.scalatest._
import scallion._
import scallion.properties._

class RecursionTests extends FlatSpec with Parsers with LL1Properties with Recursion with BooleanSyntaxes {
  import Syntax._

  "Is recursive" should "do its job" in {
    assert(isRecursive(recursive{ tru ~ falz }))
    assert(!isRecursive(tru ~ falz))
  }

  "Recursives listing" should "correctly work on trivial examples" in {
    val rec: Syntax[Boolean] = recursive( epsT )
    val expected: List[Syntax[_]] = List(rec)

    val grammars = List(
      rec,
      any ~ rec,
      rec ~ any,
      any ~ rec ~ any,
      tru || (rec ~ rec)
    )
    for(grammar <- grammars)
      assertResult(expected)(listRecursives(grammar))
  }

  it should "return an empty list if there are no recursives" in {
    val grammar = (any ~ epsT).map(orComb).mark("Marked!") | failure

    assertResult(List())(listRecursives(grammar))
  }

  it should "find nested recursives" in {
    lazy val rec1: Syntax[Boolean] = recursive( rec2 )
    lazy val rec2: Syntax[Boolean] = recursive( rec1 )

    assertResult(Set(rec1, rec2))(listRecursives(rec1).toSet)
  }

  it should "correctly find all recursives in example" in {
    lazy val rec1: Syntax[Boolean] = recursive( epsT )
    lazy val rec2: Syntax[Boolean] = recursive( (rec2 ~ tru).map(orComb) )
    lazy val rec3: Syntax[Boolean] = recursive( rec3 )
    lazy val rec4: Syntax[Boolean] = recursive( (any ~ rec4).map(andComb) )
    lazy val rec5: Syntax[Boolean] = recursive( rec1 | rec2 | rec5 )
    lazy val rec6: Syntax[Boolean] = recursive( ((rec3 ~ rec6).map(orComb) ~ rec4).map(andComb)  )

    val allRecs: Set[Syntax[_]] = Set(rec1, rec2, rec3, rec4, rec5, rec6)

    val grammar = rec5 ~ rec6
    assertResult(allRecs)(listRecursives(grammar).toSet)
  }

  it should "list the recursives in expected (depth;left-to-right) order" in {
    def genLeaf: Syntax[Boolean] = recursive( any )

    lazy val rec1 = recursive( any ~ rec2 ~ rec3 ~ any || rec4 )
    lazy val rec2 = recursive( any ~ rec5 || epsT )
    lazy val rec3 = recursive( rec6 ~ rec7 )
    lazy val rec4 = genLeaf
    lazy val rec5 = genLeaf
    lazy val rec6 = genLeaf
    lazy val rec7: Syntax[Boolean] = recursive( rec6 )
    val grammar = tru ~ rec1 ~ any

    val allRecs: List[Syntax[_]] = List(rec1, rec2, rec3, rec4, rec5, rec6, rec7)
    assertResult(allRecs)(listRecursives(grammar))
  }

  "Left recursion" should "detect simple left recursion" in {
    lazy val grammar: Recursive[Boolean] = recursive( (grammar ~ tru).map(andComb) ).asInstanceOf[Recursive[Boolean]]
    assertResult(Set(grammar))(findLeftRecursives(grammar))
    assertResult(true)(hasLeftRecursion(grammar))
    assertResult(true)(isLeftRecursive(grammar))
    assertResult(true)(isDirectLeftRecursive(grammar))
  }

  it should "detect no left recursion when there is none" in {
    lazy val grammar: Recursive[Boolean] = recursive( (falz ~ tru).map(andComb) ).asInstanceOf[Recursive[Boolean]]
    assertResult(Set.empty)(findLeftRecursives(grammar))
    assertResult(false)(hasLeftRecursion(grammar))
    assertResult(false)(isLeftRecursive(grammar))
    assertResult(false)(isDirectLeftRecursive(grammar))
  }

  it should "detect mutual recursions" in {
    lazy val rec1: Recursive[Boolean] = recursive(rec2).asInstanceOf[Recursive[Boolean]]
    lazy val rec2: Recursive[Boolean] = recursive(rec1).asInstanceOf[Recursive[Boolean]]

    assertResult(Set(rec1, rec2))(findLeftRecursives(rec1))
    assertResult(true)(hasLeftRecursion(rec1))

    assertResult(true)(isLeftRecursive(rec1))
    assertResult(false)(isDirectLeftRecursive(rec1))
    assertResult(true)(isLeftRecursive(rec2))
    assertResult(false)(isDirectLeftRecursive(rec2))
  }

  it should "detect left recursion on the right side" in {
    lazy val rec: Syntax[Boolean ~ Boolean] = recursive(rec.map(andComb) ~ tru)
    val grammar = (tru | epsT) ~ falz ~ rec.map(andComb)

    assertResult(Set(rec))(findLeftRecursives(grammar))
    assertResult(true)(hasLeftRecursion(grammar))
  }

  it should "detect all left recursions in example" in {
    lazy val rec1: Syntax[Boolean] = 
      recursive( ((rec1 ~ tru).map(andComb) ~ rec2).map(andComb) | fakeRec | rightRec | rec4 )
    lazy val rec2: Syntax[Boolean] = recursive( (rec3 ~ falz).map(andComb) )
    lazy val rec3: Syntax[Boolean] = recursive( (rec2 ~ tru).map(andComb) )
    lazy val fakeRec: Syntax[Boolean] = recursive( (tru ~ falz).map(andComb) )
    lazy val rightRec: Syntax[Boolean] = recursive( (tru ~ rightRec).map(andComb) )
    lazy val rec4: Syntax[Boolean] = 
      recursive( (rec4 ~ fakeRec).map(andComb) | (rightRec ~ rec4).map(andComb) )

    val grammar = rec1
    assertResult(
      Set(rec1, rec2, rec3, rec4)
    )(
      findLeftRecursives(grammar)
    )
    assertResult(true)(hasLeftRecursion(grammar))
  }

}