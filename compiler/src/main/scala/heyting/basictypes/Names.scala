package heyting
package basictypes

import heyting.basictypes.Types._
/**
 * User: arjan
 */
trait Ident {
  def name: String
  def typed(typ: Type): Ident = this match {
    case Raw(n) => Id(n, typ)
    case UniqName(n, u) => UId(n, u, typ)
    case Id(n, tpe) => Id(n, typ)
    case UId(n, u, tpe) => Id(n, typ)
  }
}

/**The name as it occurs in the source code.*/
case class Raw(name: String) extends Ident {
  def uniq(u: Unique): UniqName = UniqName(name, u)
}

/**A name which is made unique with respect to its environment.*/
case class UniqName(name: String, uniq: Unique) extends Ident
case class Id(name: String, tpe: Type) extends Ident {
  def uniq(u: Unique): UId = UId(name, u, tpe)
}
case class UId(name: String, uniq: Unique, tpe: Type) extends Ident

object Ident {
  implicit def raw(s: String): Raw = Raw(s)
  def uniq(s: String, u : Unique): UniqName = UniqName(s, u)

  type Name = String //simple name
}