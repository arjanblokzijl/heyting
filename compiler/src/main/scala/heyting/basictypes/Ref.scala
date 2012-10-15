package heyting
package basictypes

import Ref._
sealed trait Ref[A] {self =>
  //implemented like this because in some places bottom (an exception) is used as initial value, which must not be evaluated.
  protected var value: () => A

  def read: A = value()

  def write(a: => A): Unit = {value = () => a; ()}

  override def toString = "Ref <" + read + ">"
}

object Ref {
  def apply[A](a: => A): Ref[A] = new Ref[A] {
    protected var value = () => a
  }
}