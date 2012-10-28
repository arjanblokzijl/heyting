package heyting
package prettyprint

import ast._
import basictypes.RawName
import heyting.basictypes.Printing._
/**
 * User: arjan
 */
object PrintingSamples extends scala.App {

  val term = Lam(RawName("x"), Lam(RawName("y"), Lit(IntLit(2), RawName("2"))))
  val t2 = App(Var(RawName("x")), Var(RawName("y")), RawName("x y"))
  println("t1: " + docToString(termOutput.ppr(term)))
  println("t2: " + docToString(termOutput.ppr(t2)))
}
