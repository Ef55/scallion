package scallion
package visualization

import scala.language.existentials

import scala.collection.mutable.StringBuilder

trait SimpleStrings extends SyntaxesNavigation { self: Syntaxes =>
  import Syntax._

  private val Epsilon = "𝛆"
  private val Nothing = "⊥"

  def simpleRepr(s: Syntax[_]): String = {
    type Lines = List[StringBuilder]

    def lines(name: String) = {
      List(new StringBuilder().append(name))
    }

    def prepend(lines: Lines, prefix: String): Lines = {
      lines.map(builder => new StringBuilder().append(prefix).append(builder))
    }
    def indent(lines: Lines): Lines = {
      prepend(
        lines.map(builder => 
          if(!builder.head.isWhitespace){
            new StringBuilder().append("-").append(builder)
          }
          else{
            new StringBuilder().append(" ").append(builder)
          }
        ), 
        " |"
      )
    }

    def iter(s: Syntax[_]): Lines = s match {
      case Elem(k) => lines(k.toString)
      case Success(_) => lines(Epsilon)
      case Failure()  => lines(Nothing)

      case Sequence(l, r) => 
        lines("Sequence") ++ indent(iter(l) ++ iter(r))

      case Disjunction(l, r) => 
        lines("Disjunction") ++ indent(iter(l) ++ iter(r))

      case Transform(_, _, inner) =>
        lines("Transform") ++ indent(iter(inner))

      case Marked(mark, inner) =>
        lines("Mark " + mark) ++ indent(iter(inner))

      case Recursive(id, inner) =>
        lines("Rec " + id)

    }

    iter(s).foldLeft(new StringBuilder())((builder, line) => builder.append(line).append('\n')).toString()
  }
}