package scallion
package properties

/** Contains functions giving basic LL1 properties of syntaxes.
  *
  * @groupname property Syntaxes property
  */
trait LL1Properties{ self: Syntaxes with Parsers =>

  /** Indicate if a syntax is nullable.
   * 
   * @group property
   */
  def isNullable(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    syntaxToLL1Properties(syntax).isNullable
  }


  /** Indicate if a syntax is null (i.e. only matches the empty sequence of tokens).
   * 
   * @group property
   */
  def isNull(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    val props = syntaxToLL1Properties(syntax)
    props.isNullable && props.first.isEmpty
  }


  /** Indicate if a syntax is productive.
   * 
   * @group property
   */
  def isProductive(syntax: Syntax[A] forSome{ type A; }): Boolean = {
    syntaxToLL1Properties(syntax).isProductive
  }


  /** Return the conflicts in a syntax.
   * 
   * @group property
   */
  def getConflicts(syntax: Syntax[A] forSome{ type A; }): Set[Conflict] = {
    syntaxToLL1Properties(syntax).conflicts
  }

}