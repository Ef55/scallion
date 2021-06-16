package scallion
package transformation

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

  it should "generate non direct left recursives even with crossed recursion" in {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( ((b ~ tru) ).map(orComb) | epsF).mark("A") }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb).mark("B") }.asInstanceOf[Recursive[Boolean]]
    val grammar = a

    val factored = eliminateDirectLeftRecursions(grammar)

    for(rec <- listRecursives(factored)){
      assert(!isDirectLeftRecursive(rec), s"Direct left recursive ${rec.id}: \n${simpleRepr(rec.inner)}")
    }
  }

  "Left recursion elimination step" should "do nothing if the syntax has no left recursion" in {
    lazy val syntax1 = any ~ epsT
    lazy val syntax2: Syntax[Boolean] = (any ~ recursive(syntax2)).map(orComb)

    assertStructuralEquivalence(syntax1)(eliminateLeftRecursionsStep(syntax1))
    assertStructuralEquivalence(syntax2)(eliminateLeftRecursionsStep(syntax2))
  }

  it should "solve `chain 2` example in two iterations" in {
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

  it should "solve `two cycles` example in two iterations" in {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( ((b ~ tru) ).map(orComb) | epsF).mark("A") }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb).mark("B") }.asInstanceOf[Recursive[Boolean]]
    val grammar = a // Equivalent to (false | true)*

    assertHasConflicts(grammar)

    val step1 = eliminateLeftRecursionsStep(grammar)
    assertHasConflicts(step1)

    val step2 = eliminateLeftRecursionsStep(step1)
    val parser = assertIsLL1(step2)

    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MediumSample)
  }

  "Left recursion elimination" should "eliminate indirect left recursion without changing the underlying language" in {
    lazy val a: Recursive[Boolean] = recursive{ (b ~ tru).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = recursive{ (a ~ falz).map(orComb) }.asInstanceOf[Recursive[Boolean]]

    val grammar = a
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.SmallSample)
  }

  it should "eliminate (in)direct left recursion without changing the underlying language" in {
    lazy val a: Recursive[Boolean] = 
      recursive{ ( (b ~ tru) | (a ~ tru) ).map(orComb) | epsF }.asInstanceOf[Recursive[Boolean]]
    lazy val b: Recursive[Boolean] = 
      recursive{ ( (a ~ falz) | (b ~ falz) ).map(orComb) }.asInstanceOf[Recursive[Boolean]]
    val grammar = a
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.MinusculeSample)
  }
}

class LeftRecursionTests2 extends ParsersTestHelper with LeftRecursion with AlphabetSyntaxes {
  "Left recursion elimination" should "make complexe left recursive syntax LL1" in {
    lazy val a: Syntax[String] = 
      recursive{ ( (b ~ 'a') | (a ~ 'a') ).map(concat) | eps }
    lazy val b: Syntax[String] = 
      recursive{ ( (a ~ 'b') | (b ~ 'b') ).map(concat) }

    lazy val c: Syntax[String] = 
      recursive{ ( (d ~ 'c') ).map(concat) | 'c' }
    lazy val d: Syntax[String] = 
      recursive{ ( (c ~ 'd') | (d ~ 'd') ).map(concat) }

    lazy val e: Syntax[String] = 
      recursive{ ( (f ~ 'e') ).map(concat) | eps }
    lazy val f: Syntax[String] = 
      recursive{ ( (g ~ 'f') ).map(concat) }
    lazy val g: Syntax[String] = 
      recursive{ ( (h ~ 'g') | (e ~ 'g') ).map(concat) }
    lazy val h: Syntax[String] = 
      recursive{ ( (e ~ 'h') ).map(concat) }

    val grammar = a ~ 'X' ~ 't' | 'Y' ~ c ~ 't'  | e ~ 'Z' ~ a
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.SmallSample)
    outputGraph(factorized, "without_final")
  }
}