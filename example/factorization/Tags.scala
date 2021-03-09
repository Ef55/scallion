package example.factorization.tags

import scala.util.{Try, Success, Failure}
import scallion._
import scallion.factorization._
import silex._

/** In this example, we show how left factorization can
  * be used to transform a non-LL1 grammar into an LL1 grammar.
  * 
  * We will parse tags, as used in some markup languages (XML, HTML, ...).
  * The different "types" of tags are:
  * - Opening:  <tag>
  * - Closing:  </tag>
  * - Empty:    <tag />
  */

sealed trait Token 
case class WordToken(word: String) extends Token
case object SpaceToken extends Token 
case object SlashToken extends Token
case class AngleBracket(opening: Boolean) extends Token
case class UnknownToken(token: String) extends Token

object TagsLexer extends Lexers with CharRegExps {

  type Token = example.factorization.tags.Token  // Tokens.
  type Position = Unit  // Positions. Ignored here.

  val lexer = Lexer(
    // Words
    many1(elem(_.isLetterOrDigit)) |> { cs => WordToken(cs.mkString) },

    // Spaces
    many1(whiteSpace) |> SpaceToken,

    // Slashes
    elem('/') |> SlashToken,

    // Angle brackets
    elem('<') |> AngleBracket(true),
    elem('>') |> AngleBracket(false),

  ) onError {
    (cs, _) => UnknownToken(cs.mkString)
  }


  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, NoPositioner)

    val tokens = lexer(source)

    tokens.filter((token: Token) => token != SpaceToken)
  }

  def unapply(it: Iterable[Token]): String = {
    it.mkString("")
  }
}

sealed abstract class TokenKind(text: String) {
  override def toString = text
}
case object WordKind extends TokenKind("<word>")
case object SlashKind extends TokenKind("/")
case object LeftBracketKind extends TokenKind("<")
case object RightBracketKind extends TokenKind(">")
case object OtherKind extends TokenKind("??")

// "Incomplete" tags
case class OpeningTag(identifier: String){
  override def toString = s"<${identifier}>"
}
case class ClosingTag(identifier: String){
  override def toString = s"</${identifier}>"
}

sealed trait Tag
case class TagPair(opening: OpeningTag, closing: ClosingTag) extends Tag{
  override def toString = s"${opening}${closing}"
}
case class EmptyTag(identifier: String) extends Tag{
  override def toString = s"<${identifier} />"
}


object TagsParser extends Parsers with Factorization {
  type Token = example.factorization.tags.Token
  type Kind = example.factorization.tags.TokenKind

  import Implicits._

  // Returns the kind of tokens.
  override def getKind(token: Token): TokenKind = token match {
    case WordToken(w)         => WordKind
    case SlashToken           => SlashKind
    case AngleBracket(true)   => LeftBracketKind
    case AngleBracket(false)  => RightBracketKind
    case _                    => OtherKind
  }

  // Basic blocks
  val identifier =  accept(WordKind){ case WordToken(id) => id }
  val tagPrefix =   elem(LeftBracketKind)
  val tagSuffix =   elem(RightBracketKind)
  val slash =       elem(SlashKind)

  // The different tags
  val openingTag =  (tagPrefix ~ identifier ~<~ tagSuffix).map{case _ ~ id => OpeningTag(id)}
  val closingTag =  (tagPrefix ~>~ slash ~>~ identifier ~<~ tagSuffix).map(id => ClosingTag(id))
  val emptyTag =    (tagPrefix ~ identifier ~<~ slash ~<~ tagSuffix).map{case _ ~ id => EmptyTag(id)}.up[Tag]
  val tagPair =     (openingTag ~ closingTag).map{ case op ~ cl => TagPair(op, cl)}.up[Tag]

  // This is clearly not LL1: both alternatives begin with `tagPrefix ~ identifier`
  val grammar =   tagPair | emptyTag     


  // Trying to generate a parser "as-is"
  val parser = Try(Parser(grammar))

  // With factorization
  val parserFactorized = Try(Parser( leftFactorize(tagPrefix ~ identifier, grammar) ))

}


object tags {
  def main(args: Array[String]) {
    val examples = Seq(
      // Valid
      "<tag>  </tag>",
      "<tag />",
      "<h3  >     </  h3>",
      // Invalide
      ">tag<",
      "This input shouldn't even be here",
      "<tag tag><tag / tag>",
      "<tag><tag />",
      "</tag>",
    )

    val Failure(f) = TagsParser.parser
    println("A conflict was detected in the first syntax !")


    val Success(parser) = TagsParser.parserFactorized
    println("No conflicts were detected in the second syntax !")
    for(sentence <- examples){
      parser(TagsLexer(sentence.iterator)) match {
        case TagsParser.Parsed(r, _)  => println(s"Parsed: `${sentence}` as `${r}`")
        case _                        => println(s"Rejected: `${sentence}`")
      }
    }

    // Output:
    // ===================================================
    // A conflict was detected in the first syntax !
    // No conflicts were detected in the second syntax !
    // Parsed: `<tag>  </tag>` as `<tag></tag>`
    // Parsed: `<tag />` as `<tag />`
    // Parsed: `<h3  >     </  h3>` as `<h3></h3>`
    // Rejected: `>tag<`
    // Rejected: `This input shouldn't even be here`
    // Rejected: `<tag tag><tag / tag>`
    // Rejected: `<tag><tag />`
    // Rejected: `</tag>`
    // ===================================================
  }
}