package heyting
package ast

import basictypes.Ident

/**
 * User: arjan
 */
sealed trait HsBinds extends Tree
case class HsLocalBind(id: Ident, binding: Literal) extends HsBinds
