package heyting.basictypes

/**
 * User: arjan
 */
trait UniqSupply[A] {
  protected var value: A

  def read: A = value
}
