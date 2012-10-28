package heyting
package basictypes

/**
 * User: arjan
 */
class Unique(u: Int = 0) {
  def incr: Unique = new Unique(u + 1)

  def read: Int = u

  override def toString: String = u.toString
}

object Unique {
  def newUnique: Unique = new Unique
}