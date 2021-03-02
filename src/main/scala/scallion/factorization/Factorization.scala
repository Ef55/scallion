package scallion.factorization

import scallion.Syntaxes

/** Contains functions to factorize syntaxes. */
trait Factorization extends LeftFactorization with Substitution{
  self: Syntaxes => 
}