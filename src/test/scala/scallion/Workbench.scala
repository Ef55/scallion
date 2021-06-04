import scallion._
import scallion.factorization._
import scala.collection.immutable.NumericRange
import org.scalatest._
import org.scalatest.tagobjects._

class Workbench extends ParsersTestHelper with LeftRecursion with AlphabetSyntaxes {
  "Left recursion elimination" should "not loop infinitely" ignore {
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
      recursive{ ( (h ~ 'g') | (f ~ 'g') ).map(concat) } // This causes an infinite loop...
    lazy val h: Syntax[String] = 
      recursive{ ( (e ~ 'h') ).map(concat) }

    val grammar = a ~ 'X' ~ 't' | 'Y' ~ c ~ 't'  | e ~ 'Z' ~ a
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.SmallSample)
    outputGraph(factorized, "without_final")
  }

  "End-to-end" should "make syntax LL1" in {
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

    val grammar = a ~ 'X' ~ 't' | 'Y' ~ c ~ 't'  | e ~ 'Z' ~ a | 'A' ~ b ~ 't'
    val factorized = eliminateLeftRecursions(grammar)

    assertHasConflicts(grammar)
    val parser = assertIsLL1(factorized)
    
    assertParses(getValue(_), valuePrinter(_))(parser, grammar, ParsersTestHelper.SmallSample)
    outputGraph(factorized, "without_final")
  }
}