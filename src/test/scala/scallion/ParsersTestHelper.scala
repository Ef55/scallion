package scallion

import scallion.properties._
import scallion.visualization._
import scallion.properties.StructuralEquivalence
import scallion.util.Logger
import org.scalatest._
import scala.util.Random
import scala.collection.mutable.{Set, HashMap, MultiMap}

trait ParsersTestHelper extends FlatSpec with Inside with BeforeAndAfter
with Parsers with LL1Properties with Graphs with Grammars with SimpleStrings with Enumeration with StructuralEquivalence {
  import ParsersTestHelper._

  before {
    Logger.clear
  }

  def generateSample[A](valuer: Kind => Token, printer: Token => String = _.toString)(generator: Syntax[A])(count: SampleSize): List[Iterator[Token]] = {
    val samples = Enumerator(generator).take(count.value + PrintSamples).toList.map(_.map(valuer))
    if(PrintSamples > 0){
      println(s"Random samples (among ${samples.size}): ")
      val (printed, rem) = RNG.shuffle(samples).splitAt(PrintSamples)
      printed.foreach(
        sample => println(sample.map(printer).mkString)
      )
      rem
    }
    else{
      samples
    }
  }

  def assertHasConflicts(syntax: Syntax[_]): Unit = {
    assertThrows[ConflictException](Parser(syntax))
  }

  def assertIsLL1[A](syntax: Syntax[A]): Parser[A] = {
    try{
      Parser(syntax)
    }
    catch{
      case ConflictException(conflicts) => {
        outputGraph(syntax, s"${suiteName}_unexpectedly_not_ll1")
        fail(if(PrintConflictsReport){ debugString(syntax) }else{ "The syntax is not LL1 !" })
      }
    }
  }

  def assertParses[A](parser: Parser[A], input: Iterator[Token]): A = {
    val (i1, i2) = input.duplicate
    parser(i1) match {
      case Parsed(r, _)   => r
      case _              => fail(s"Parsing failed on `${i2.toList}`")
    }
  }

  def assertParses[A](parser: Parser[A], input: Seq[Token]): A = assertParses(parser, input.iterator)

  def assertParses[A](parser: Parser[A], inputs: Seq[Seq[Token]]): Seq[A] = {
    inputs.map{
      case input => assertParses(parser, input)
    }
  }

  def assertParses[A, I](parser: Parser[A], inputs: Seq[I], lexer: I => Seq[Token]): Seq[A] = {
    assertParses(parser, inputs.map(lexer(_)))
  }



  def assertParses[A](valuer: Kind => Token, printer: Token => String = _.toString)(parser: Parser[A], generator: Syntax[A], count: SampleSize): Seq[A] = {
    generateSample(valuer, printer)(generator)(count).map(input => assertParses(parser, input))
  }

  def assertParseResult[A](expected: A)(parser: Parser[A], input: Seq[Token]): A = {
    val result = assertParses(parser, input)
    assertResult(expected, s"on input `${input}`")(result)
    result
  }

  def assertParseResults[A](parser: Parser[A], inputsExpected: Seq[(Seq[Token], A)]): Seq[A] = {
    inputsExpected.map{
      case (input, expected) => assertParseResult(expected)(parser, input)
    }
  }

  def assertParseResults[A, I](parser: Parser[A], inputsExpected: Seq[(I, A)], lexer: I => Seq[Token]): Seq[A] = {
    assertParseResults(parser, inputsExpected.map(p => (lexer(p._1), p._2)))
  }

  def assertStructuralEquivalence[A](expected: Syntax[A])(actual: Syntax[A]): Syntax[A] = {
    import Syntax._
    val recs = new HashMap[RecId, Set[RecId]] with MultiMap[RecId, RecId]

    def constructName(s: Syntax[_]): String = {
      s match {
        case Elem(e)  => s"Elem(${e})"
        case Sequence(_, _) => "Sequence"
        case Disjunction(_, _) => "Disjunction"
        case Transform(_, _, _) => "Transform"
        case Marked(m, _) => s"Marked(${m})"
        case Success(s) => s"Success(${s})"
        case Failure()  => "Failure"
        case Recursive(id, _) => s"Recursive(${id})"
      }
    }

    def iter(actual: Syntax[_], expected: Syntax[_], str: String): Unit = {
      val sep = "\n\t- "
      (actual, expected) match {
        case (Elem(e1), Elem(e2)) => 
          assert(e1 == e2, s"${str}${sep}Element mismatch: `${e1}` is not `${e2}`")
        case (Sequence(l1, r1), Sequence(l2, r2)) => { 
          iter(l1, l2, s"${str}${sep}Sequence (left)")
          iter(r1, r2, s"${str}${sep}Sequence (right)") 
        }
        case (Disjunction(l1, r1), Disjunction(l2, r2)) => { 
          iter(l1, l2, s"${str}${sep}Disjunction (left)")
          iter(r1, r2, s"${str}${sep}Disjunction (right)") 
        }
        case (Transform(_, _, i1), Transform(_, _, i2)) => 
          iter(i1, i2, s"${str}${sep}Transform")
        case (Marked(m1, i1), Marked(m2, i2)) => 
          iter(i1, i2, s"${str}${sep}Marked (`${m1}` and `${m2}`)")
        case (Success(s1), Success(s2)) => 
          assert(s1 == s2, s"${str}${sep}Success mismatch: `${s1}` is not `${s2}`")
        case (Failure(), Failure()) =>
          ()
        case (Recursive(id1, in1), Recursive(id2, in2)) => 
          if(recs.entryExists(id1, _ == id2)){
            ()
          }
          else{
            recs.addBinding(id1, id2)
            iter(in1, in2, s"${str}${sep}Recursive (${id1} and ${id2})")
          }
        case _ => 
          fail(s"${str}${sep}Construct mismatch: `${constructName(actual)}` is not `${constructName(expected)}`")
      }
    }

    try{
      iter(expected, actual, "Syntaxes not equivalent: ")
    }
    catch{
      case e: Throwable if OutputGraphsOnFailure => {
        outputGraph(expected, s"${suiteName}_expected")
        outputGraph(actual, s"${suiteName}_actual")
        throw e
      }
      case e: Throwable => throw e
    }

    // Additional check to ensure coherence
    assert(structurallyEquivalent(actual, expected))
    actual
  }

  def assertStructuralEquivalenceUntyped[A](expected: Syntax[_])(actual: Syntax[A]): Syntax[A] = {
    assert(structurallyEquivalent(actual, expected))
    actual
  }

  def outputGraph[A](syntax: Syntax[A], name: String): Unit = {
    graphs.outputGraph(syntax, GraphDirectory, name)
  }

  def printGrammar[A](syntax: Syntax[A], name: String = "<unspecified>"): Unit = {
    println(s"Grammar ${name}:")
    println(grammars.getGrammar(syntax).pretty())
  }

  def dumpLogs: Unit = {
    assert(Logger.Enabled)
    Logger.print
    Logger.clear
  }
}

object ParsersTestHelper {
  final case class SampleSize(value: Int)

  val EmptySample = SampleSize(0)
  val MinusculeSample = SampleSize(10)
  val TinySample = SampleSize(500)
  val SmallSample = SampleSize(1000)
  val MediumSample = SampleSize(3000)
  val BigSample = SampleSize(5000)
  val HugeSample = SampleSize(10000)

  val PrintConflictsReport: Boolean = true

  val RNG: Random = new Random(123854514)
  val PrintSamples: Int = 0
  require(PrintSamples >= 0)

  val OutputGraphsOnFailure = true
  val GraphDirectory = "target/graphs"
}