package heyting
package prettyprint

import ast._
import basictypes.Raw
import heyting.basictypes.Printing._
/**
 * User: arjan
 */
object PrintingSamples extends scala.App {

  val term = Lam(Raw("x"), Lam(Raw("y"), Lit(IntLit(2), Raw("2"))))
  val t2 = App(Var(Raw("x")), Var(Raw("y")), Raw("x y"))
  println("t1: " + docToString(termOutput.ppr(term)))
  println("t2: " + docToString(termOutput.ppr(t2)))
}
