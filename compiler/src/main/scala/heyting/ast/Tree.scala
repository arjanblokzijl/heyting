package heyting
package ast

import basictypes.Ident
import basictypes.Types._
/**
 * User: arjan
 */
trait Tree
sealed trait Literal extends Tree
case class IntLit(i: Int) extends Literal
case class StringLit(s: String) extends Literal
case class CharLit(c: Char) extends Literal
case class DoubleLit(d: Double) extends Literal
//case class IdentLit(id: Ident) extends Literal

sealed trait Term extends Tree {
  def atomicTerm: Boolean = this match {
    case Var(_) => true
    case Lit(_) => true
    case _ => false
  }
}

case class Var(n: Ident) extends Term
case class Lit(l: Literal) extends Term
case class App(l: Term, r: Term) extends Term
case class Lam(n: Ident, t: Term) extends Term
case class ALam(n: Ident, s: Sigma, t: Term) extends Term
case class Let(value: Ident, expr: Term) extends Term
case class Ann(t: Term, s: Sigma) extends Term

object EmptyTree extends Tree {
  override def toString = "<empty>"
}

object Term {

}