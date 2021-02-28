import scallion._
import scallion.visualization._
import scallion.factorization._
import scallion.util._

object Main extends Parsers with LeftFactorization with Grammars {
    type Token = Char
    type Kind = Boolean

    import scala.language.implicitConversions

    override def getKind(t: Token): Kind = t.isLetter

    val letter = elem(true)
    val other = elem(false)
    val s = (letter ~ letter).map(_ => 0) | (letter ~ other).map(_ => 1) | (other ~ other).map(_ => 2)
    val program = leftFactor(true, s)

    def main(args: Array[String]) = {
        val input = "11"

        println(grammars.getGrammar(program).pretty())

        if(!program.isLL1){
            println("Not LL1")
            debug(program, true)
        }
        else{
            val parser = Parser(program)

            parser(input.iterator) match {
                case Parsed(r, _) =>
                    println(s"Parsed: ${r}")
                case _  => println("Parsing failed")
            }
        }
    }
}