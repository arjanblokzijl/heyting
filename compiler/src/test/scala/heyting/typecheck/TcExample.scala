package heyting
package typecheck

import ast._
import typing._
import basictypes._
import Types._
import heyting.prettyprint.Docs._
import Printing._
import Ident._
/**
 * User: arjan
 */
object TcExample extends scala.App {
  val emptyTypeEnv: Seq[(Ident, Sigma)] = Seq()
  val initTypeEnv: Seq[(Ident, Sigma)] =
    Seq((RawName("+"), IntT --> IntT --> IntT),
        (RawName("true"), BooleanT),
        (RawName("false"), BooleanT))


  val letExpr = Let(RawName("a"),Lit(IntLit(1), RawName("1")))
  val lexpr = Lam(raw("a"),
                    Lam(raw("b"), Var(raw("b")))
                 )
  val lexpr2 = Lam(raw("a"),Var(raw("a")))
  val expr2 = App(
                Var(RawName("add")), App(Var(RawName("a")), Var(RawName("b")), RawName("a b")), RawName("add"))

  val letExpr2 = Let(RawName("fun"), lexpr)
  val expr = letExpr2
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

  println("typeCheck: " + texpr)
}
