package heyting
package typecheck

import ast._
import typing._
import basictypes._
import Types._
import heyting.prettyprint.Docs._
import Printing._

/**
 * User: arjan
 */
object TcExample extends scala.App {
  val emptyTypeEnv: Seq[(Ident, Sigma)] = Seq()
  val initTypeEnv: Seq[(Ident, Sigma)] =
    Seq((RawName("+"), IntT --> IntT --> IntT),
        (RawName("true"), BooleanT),
        (RawName("false"), BooleanT))


  val expr = Let(RawName("a"),Lit(IntLit(1)))
  val result = Tc.runTc(emptyTypeEnv, TypeCheck.tcExpr(expr))
  val tpe = result match {
    case Left(err) => Printing.docToString(err)
    case Right(ty) => Printing.docToString(sep(Vector(pprParentTerm(expr), nest(2, dcolon <+> typeOutput.ppr(ty)))))
  }
  println("typeCheck: " + tpe)
}
