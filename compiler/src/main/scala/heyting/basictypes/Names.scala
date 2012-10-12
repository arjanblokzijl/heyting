package heyting
package basictypes

import compilation.Compilation._
import heyting.basictypes.Types._
/**
 * User: arjan
 */
trait Ident {
  def name: String
}

/**The name as it occurs in the source code.*/
case class RawName(name: String) extends Ident {
  def uniq(u: Unique): UniqName = UniqName(name, u)
}

/**A name which is made unique with respect to its enviroment.*/
case class UniqName(name: String, uniq: Unique) extends Ident {
  def typed(t: Type): TypedUniqName = TypedUniqName(name, uniq, t)
}
case class TypedUniqName(name: String, uniq: Unique, tpe: Type) extends Ident

object Ident {
  def raw(s: String): RawName = RawName(s)
  def uniq(s: String, u : Unique): UniqName = UniqName(s, u)

  type Name = String //simple name
}