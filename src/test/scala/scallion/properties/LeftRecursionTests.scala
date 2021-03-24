package scallion.properties

import scala.collection.immutable.Set
import org.scalatest._
import scallion._
import scallion.properties._

class LeftRecursionTests extends FlatSpec with Parsers with LeftRecursion {
  type Token = Boolean
  type Kind = Boolean

  import Syntax._

  def getKind(t: Token): Kind = t

  val tru = elem(true)
  val falz = elem(false)
  val eps = epsilon(false)
  val comb = (p: Boolean ~ Boolean) => p._1 && p._2

  "Left recursion" should "detect simple left recursion" in {
    lazy val grammar: Syntax[Boolean] = recursive( (grammar ~ tru).map(comb) )
    assertResult(Set(grammar))(findLeftRecursions(grammar))
    assertResult(true)(hasLeftRecursion(grammar))
  }

  it should "detect no left recursion when there is none" in {
    lazy val grammar: Syntax[Boolean] = recursive( (falz ~ tru).map(comb) )
    assertResult(Set.empty)(findLeftRecursions(grammar))
    assertResult(false)(hasLeftRecursion(grammar))
  }

  it should "detect mutual recursions" in {
    lazy val rec1: Syntax[Boolean] = recursive(rec2)
    lazy val rec2: Syntax[Boolean] = recursive(rec1)

    assertResult(Set(rec1, rec2))(findLeftRecursions(rec1))
    assertResult(true)(hasLeftRecursion(rec1))
  }

  it should "detect left recursion on the right side" in {
    lazy val rec: Syntax[Boolean ~ Boolean] = recursive(rec.map(comb) ~ tru)
    val grammar = (tru | eps) ~ falz ~ rec.map(comb)

    assertResult(Set(rec))(findLeftRecursions(grammar))
    assertResult(true)(hasLeftRecursion(grammar))
  }

  it should "detect all left recursions in example" in {
    lazy val rec1: Syntax[Boolean] = 
      recursive( ((rec1 ~ tru).map(comb) ~ rec2).map(comb) | fakeRec | rightRec | rec4 )
    lazy val rec2: Syntax[Boolean] = recursive( (rec3 ~ falz).map(comb) )
    lazy val rec3: Syntax[Boolean] = recursive( (rec2 ~ tru).map(comb) )
    lazy val fakeRec: Syntax[Boolean] = recursive( (tru ~ falz).map(comb) )
    lazy val rightRec: Syntax[Boolean] = recursive( (tru ~ rightRec).map(comb) )
    lazy val rec4: Syntax[Boolean] = 
      recursive( (rec4 ~ fakeRec).map(comb) | (rightRec ~ rec4).map(comb) )

    val grammar = rec1
    assertResult(
      Set(rec1, rec2, rec3, rec4)
    )(
      findLeftRecursions(grammar)
    )
    assertResult(true)(hasLeftRecursion(grammar))
  }
}