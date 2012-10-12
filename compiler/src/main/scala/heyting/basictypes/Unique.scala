package heyting
package basictypes

/**
 * User: arjan
 */
class Unique {
  private[this] var u: Int = 0
  def incr: Unique = {u+=1; this}

  def read: Int = u
}

object Unique {
  def newUnique: Unique = new Unique
}