package scallion
package properties

trait LL1Properties{ self: Syntaxes with Parsers =>

  def isNullable(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    syntaxToLL1Properties(syntax).isNullable
  }

  def isNull(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    val props = syntaxToLL1Properties(syntax)
    props.isNullable && props.first.isEmpty
  }

  def isProductive(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    syntaxToLL1Properties(syntax).isProductive
  }

  def getConflicts(syntax: Syntax[A] forSome{ type A; }): Set[Conflict] = {
    syntaxToLL1Properties(syntax).conflicts
  }

}