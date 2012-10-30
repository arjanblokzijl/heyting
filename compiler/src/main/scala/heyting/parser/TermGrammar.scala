package heyting
package parser

import util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import basictypes._
import ast._

/**
 * User: arjan
 */
trait TermGrammar extends JavaTokenParsers with PackratParsers {

  lazy val keyword: Parser[String] = "let" | "in" | "forall" | "if" | "then" | "else"
  lazy val reservedOps: Set[String] = Set("\\", "->")

  lazy val identifier: Parser[Ident] = not(keyword) ~> ident ^^ name

  lazy val int: Parser[IntLit] = wholeNumber ^^ (s => IntLit(s.toInt))

  lazy val double: Parser[Literal] = decimalNumber ^^ (s => DoubleLit(s.toDouble))

  lazy val lit: Parser[Term] = (int | double) ^^ literalLit

  lazy val variable: Parser[Term] =  not(keyword) ~> ident ^^ (s => Var(Raw(s)))

  lazy val symbol: Parser[Term] = """[\+\-\*]+""".r ^^ (s => Var(Raw(s.toString)))

  lazy val args: PackratParser[List[Ident]] = log(rep(not(keyword) ~> identifier))("args")

  lazy val atom = lit | variable | symbol | parens(readTerm)

  lazy val readTerm = log(let | app | lam)("readTerm")

  def parens(p: Parser[Term]): Parser[Term] = "(" ~> p <~ ")"

  def reservedOp(op: String): Parser[String] = if (reservedOps.contains(op)) op else failure("not a valid operator: " + op)

  lazy val let: Parser[Term] = "let" ~> (identifier ~ args <~ "=") ~ readTerm ^^ letExpr

  lazy val app: Parser[Term] =
    rep1(atom).map{case fun::args =>
      args.foldLeft(fun)((f, a) => App(f, a, Raw(f.id.name + a.id.name)))
      case Nil => sys.error("app: empty list") //impossible
    }

  lazy val lam: Parser[Term] = reservedOp("\\") ~> ord_lam

  lazy val ord_lam: Parser[Term] = (rep(identifier) <~ reservedOp("->")) ~ readTerm ^^ nestedLam

  lazy val name: String => Raw = Raw(_)

  lazy val optArgs: Option[List[Var]] => List[Var] = a => a match {
    case None => List[Var]()
    case Some(vars) => vars
  }

  lazy val letExpr: Ident ~ List[Ident] ~ Term => Let = {case id ~ vars ~ t => vars match {
    case Nil => Let(id, t)
    case x::xs => Let(id, xs.foldLeft(Lam(x, t))((l, i) => Lam(i, l)))
  }}

  lazy val nestedLam: List[Ident] ~ Term => Lam = {case vars ~ t => vars.reverse match {
    case Nil => sys.error("nestedLam: empty list")
    case x::xs => xs.foldLeft(Lam(x, t))((l, i) => Lam(i, l))
  }}

  lazy val literalLit: Literal => Lit = l => l match {
    case IntLit(v) => Lit(l, Raw(v.toString))
    case DoubleLit(v) => Lit(l, Raw(v.toString))
    case CharLit(c) => Lit(l, Raw(c.toString))
    case StringLit(s) => Lit(l, Raw(s))
  }
}
