package scallion

trait StringSyntaxes extends Syntaxes {
  sealed trait Token{
    def toChar: Char = this match {
      case Letter(c)    => c
      case Number(c)    => c 
      case Separator(c) => c 
    }
  }
  case class Letter(char: Char) extends Token
  case class Number(num: Char) extends Token
  case class Separator(sep: Char) extends Token

  sealed trait Kind
  case object LetterKind extends Kind
  case object NumberKind extends Kind
  case object SeparatorKind extends Kind
  

  object Lexer {
    def apply(input: String): Seq[Token] = {
      input.map( chr =>
        if(chr.isLetter){
          Letter(chr)
        }
        else if(chr.isDigit){
          Number(chr)
        }
        else{
          Separator(chr)
        }
      )
    }
  }

  override def getKind(token: Token): Kind = token match {
    case Letter(_)      => LetterKind
    case Number(_)      => NumberKind
    case Separator(_)   => SeparatorKind
  }

  val letter = elem(LetterKind)
  val number = elem(NumberKind)
  val inumber = number.map(n => { val Number(i) = n; i.asDigit })
  val sep = elem(SeparatorKind)
}

trait BooleanSyntaxes extends Syntaxes {
  type Token = Boolean
  type Kind = Boolean

  def getKind(t: Token): Kind = t
  def getValue(k: Kind): Token = k

  val tru = elem(true)
  val falz = elem(false)
  val any = tru | falz
  val epsT = epsilon(true)
  val epsF = epsilon(false)
  val orComb = (p: Boolean ~ Boolean) => p._1 || p._2
  val andComb = (p: Boolean ~ Boolean) => p._1 && p._2
}