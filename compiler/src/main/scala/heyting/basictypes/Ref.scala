package heyting
package basictypes

import Ref._
sealed trait Ref[A] {self =>
  protected var value: A

  def read: A = value

  def write(a: A): Unit = {value = a; ()}

  def map[B](f: A => B): Ref[B] = new Ref[B] {
    protected var value = f(self.value)
  }

  def flatMap[B](f: A => Ref[B]): Ref[B] = f(value)
}

object Ref {
  def apply[A](a: A): Ref[A] = new Ref[A] {
    protected var value = a
  }
}