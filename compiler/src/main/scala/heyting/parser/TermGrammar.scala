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

  lazy val liter: Parser[Ident] = not(keyword) ~> ident ^^ name

  lazy val argList: Parser[List[Var]] = rep(not(keyword) ~> ident ^^ varTerm)

  lazy val int: Parser[IntLit] = wholeNumber ^^ intLit

  lazy val lit: Parser[Term] = (int | double) ^^ mkLit

  lazy val variable: Parser[Term] =  not(keyword) ~> ident ^^ (s => Var(RawName(s)))

  lazy val double: Parser[Literal] = decimalNumber ^^ doubleLit

  lazy val symbol: Parser[Term] = """[\+\-\*]+""".r ^^ (s => Var(RawName(s.toString)))

  lazy val namelist: PackratParser[List[Ident]] = log(rep(liter))("names")

  lazy val args: PackratParser[List[Var]] = log("(" ~> opt(argList) <~ ")" ^^ optArgs)("args") |
    failure("Illegal arguments syntax")

  lazy val let: Parser[Term] = "let" ~> (liter ~ namelist <~ "=") ~ readTerm ^^ letExpr

  lazy val atom = lit | variable | symbol | parens(readTerm)

  def readTerm = log(let | app)("readTerm")

  def parens(p: Parser[Term]): Parser[Term] = "(" ~> p <~ ")"

  def app: Parser[Term] =
    rep1(atom).map{case fun::args =>
      args.foldLeft(fun)((f, a) => App(f, a, RawName(f.id.name + a.id.name)))
      case Nil => sys.error("app: empty list") //impossible
    }


  def name: String => RawName = RawName(_)

  def optArgs: Option[List[Var]] => List[Var] = a => a match {
    case None => List[Var]()
    case Some(vars) => vars
  }

  def letExpr: Ident ~ List[Ident] ~ Term => Let = {case id ~ args ~ t => args match {
    case Nil => Let(id, t)
    case x::xs => Let(id, xs.foldLeft(Lam(x, t))((l, i) => Lam(i, l)))
  }}


  def varTerm(s: String) = Var(RawName(s))

  def mkLit: Literal => Lit = l => l match {
    case IntLit(v) => Lit(l, RawName(v.toString))
    case DoubleLit(v) => Lit(l, RawName(v.toString))
    case CharLit(c) => Lit(l, RawName(c.toString))
    case StringLit(s) => Lit(l, RawName(s))
  }

  def mkDoubleLit: DoubleLit => Lit = d => Lit(d, RawName(d.d.toString))

  def intLit: String => IntLit = s => IntLit(s.toInt)

  def doubleLit: String => DoubleLit = s => DoubleLit(s.toDouble)

}
