package heyting
package ast

import basictypes.{UId, Id, Type, Ident}
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
  def id: Ident
  def tpe: Option[Type] = id match {
    case Id(n, tpe) => Some(tpe)
    case UId(n, u, tpe) => Some(tpe)
    case _ => None
  }
  def atomicTerm: Boolean = this match {
    case Var(_) => true
    case Lit(_,_) => true
    case _ => false
  }
  def typed(typ: Type) = {
    def tpe(id: Ident): Ident = id.typed(typ)
    this match {
      case Var(id) => Var(tpe(id))
      case Lit(l, id) => Lit(l, tpe(id))
      case App(l, r, id) => App(l, r, tpe(id))
      case Lam(id, t) => Lam(tpe(id), t)
      case ALam(n, s, t, id) => ALam(n, s, t, tpe(id))
      case Let(id, e) => Let(tpe(id), e)
      case Ann(t, s, id) => Ann(t, s, id)
    }
  }
}

case class Var(id: Ident) extends Term
case class Lit(l: Literal, id: Ident) extends Term
case class App(l: Term, r: Term, id: Ident) extends Term
case class Lam(id: Ident, t: Term) extends Term
case class ALam(n: Ident, s: Sigma, t: Term, id: Ident) extends Term
case class Let(id: Ident, expr: Term) extends Term
case class Ann(t: Term, s: Sigma, id: Ident) extends Term

object EmptyTree extends Tree {
  override def toString = "<empty>"
}

object Term {

}