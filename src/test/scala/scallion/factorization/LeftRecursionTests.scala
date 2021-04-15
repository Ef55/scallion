package scallion
package factorization

import scala.collection.immutable.NumericRange
import org.scalatest._
import org.scalatest.tagobjects._

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
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "eliminate direct left recursion even with null prefix" in {
    lazy val grammar: Recursive[Boolean] = recursive( (epsT ~ (grammar ~ any).map(orComb)).map(andComb) | epsF ).asInstanceOf[Recursive[Boolean]]
    val factorized = eliminateDirectLeftRecursion(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    val inputs = Seq(
      (Seq(), false),
      (Seq(false, false), false),
      (Seq(true), true)
    )

    assertParseResults(parser, inputs)
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  it should "do nothing if the syntax has no direct recursion" in {
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(orComb) }.asInstanceOf[Recursive[Boolean]]

    val grammar = a
    assertResult(grammar)(eliminateDirectLeftRecursion(grammar))
  }

  "Left recursion elimination step" should "do nothing if the syntax has no left recursion" in {
    lazy val syntax1 = any ~ epsT
    lazy val syntax2: Syntax[Boolean] = (any ~ recursive(syntax2)).map(orComb)

    assertStructuralEquivalence(syntax1)(eliminateLeftRecursionsStep(syntax1))
    assertStructuralEquivalence(syntax2)(eliminateLeftRecursionsStep(syntax2))
  }

  it should "solve example in two iterations" in {
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(orComb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a

    assertHasConflicts(a)

    val step1 = eliminateLeftRecursionsStep(grammar)
    assertHasConflicts(step1)
    val recs = listRecursives(step1)
    assert(recs.size > 0)
    recs.foreach(r => assert(isDirectLeftRecursive(r)))

    val step2 = eliminateLeftRecursionsStep(step1)
    val parser = assertIsLL1(step2)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  def solveChainRecursion(depth: Int, sample: ParsersTestHelper.SampleSize): Unit = {
    it should s"solve a recursion-chain of depth ${depth} in ${depth} steps" taggedAs(Slow) in {
      require(depth > 0)
      val range = NumericRange(0, depth, 1)
      val nodes = new Array[Syntax[Boolean]](depth)
      for(i <- range){
        val next = (i+1) % depth
        nodes(i) = recursive{ 
          if(i == 0){ (nodes(next) ~ tru).map(orComb) | epsT }
          else{ (nodes(next) ~ tru).map(orComb) } 
        }
      }
      val grammar = nodes(0)


      var result = grammar
      for(i <- range){
        assertHasConflicts(result)
        assert(isLeftRecursive(result.asInstanceOf[Recursive[Boolean]]))
        result = eliminateLeftRecursionsStep(result)
      }

      val parser = assertIsLL1(result)
      assertParses(getValue(_), valuePrinter(_))(parser, grammar, sample)
    }
  }
  
  it should behave like solveChainRecursion(5, ParsersTestHelper.SmallSample)

  it should behave like solveChainRecursion(20, ParsersTestHelper.MinusculeSample)

  it should "solve example in XXX iterations" ignore {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( ((b ~ tru) | (a ~ tru) ).map(orComb) | epsF).mark("A") }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb).mark("B") }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false | true)*

    assertHasConflicts(grammar)
    outputGraph(grammar, "grammar")
    printGrammar(grammar, "grammar")

    // val step1 = eliminateLeftRecursionsStep(grammar)
    // outputGraph(step1, "step1")
    // printGrammar(step1, "step1")
    // assertHasConflicts(step1)

    // val step2 = eliminateLeftRecursionsStep(step1)
    // outputGraph(step2, "step2")
    // printGrammar(step2, "step2")
    // val parser = assertIsLL1(step2)
    val step1 = eliminateDirectLeftRecursions(grammar)
    outputGraph(step1, "step1")
    val step2 = unfoldLeftmostRecursivesOfLeftRecursives(step1)
    outputGraph(step2, "step2")
    val step3 = eliminateDirectLeftRecursions(step2)
    outputGraph(step3, "step3")
    val step4 = unfoldLeftmostRecursivesOfLeftRecursives(step3)
    outputGraph(step4, "step4")

    val parser = assertIsLL1(step4)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  "Left recursion elimination" should "eliminate indirect left recursion without changing the underlying language" in {
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(orComb) }.asInstanceOf[Recursive[Boolean]]

    val grammar = a // Equivalent to (false ~ true)*
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.SmallSample)
  }

  it should "eliminate (in)direct left recursion without changing the underlying language" ignore {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( (b ~ tru) | (a ~ tru) ).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false | true)*
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MinusculeSample)
  }
}