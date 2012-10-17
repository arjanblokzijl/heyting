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


  val expr = Let(RawName("a"),Lit(IntLit(1), RawName("1")))
  val result = Tc.runTc(emptyTypeEnv, TypeCheck.tcExpr(expr))
  val texpr = result match {
    case Left(err) => Printing.docToString(err)
    case Right(ex) => {
      ex.tpe match {
        case Some(ty)  => Printing.docToString(sep(Vector(pprParentTerm(expr), nest(2, dcolon <+> typeOutput.ppr(ty)))))
        case None => Printing.docToString(text("typing error: expr ") <+> termOutput.ppr(ex) <+> nest(2, text("could not be typed")))
      }
    }
  }

  println("typeCheck expressing " + texpr)
}
