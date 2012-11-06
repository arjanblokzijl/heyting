package heyting
package parser

import util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import basictypes._
import ast._
import heyting.basictypes.Types._

/**
 * User: arjan
 */
trait TermGrammar extends JavaTokenParsers with PackratParsers {
//  override val whiteSpace = """[ \t]+""".r
  def eol = """(\r?\n)+""".r

  def keyword: Parser[String] = "let" | "in" | "forall" | "if" | "then" | "else"

  def keywords: Set[String] = Set("let", "in", "forall", "if", "then", "else")

  def reservedOps: Set[String] = Set("\\", "->", "::")

  def identifier: Parser[Ident] = not(keyword) ~> ident ^^ Raw

  def int: Parser[IntLit] = wholeNumber ^^ (s => IntLit(s.toInt))

  def double: Parser[Literal] = decimalNumber ^^ (s => DoubleLit(s.toDouble))

  def lit: Parser[Term] = (int | double) ^^ literalLit

  def variable: Parser[Term] =  not(keyword) ~> ident ^^ (s => Var(Raw(s)))

  def symbol: Parser[Term] = """[\+\-\*]+""".r ^^ (s => Var(Raw(s.toString)))

  def args: PackratParser[List[Ident]] = log(rep(not(keyword) ~> identifier))("args")

  def atom = lit | variable | symbol | parens(term)

  def arrow: Parser[String] = reservedOp("->")

  def terms: PackratParser[List[Term]] = log(rep(term  <~ opt(eol)))("terms")

  def term: PackratParser[Term] = log((ann | non_ann))("term")

  def non_ann = (let | app | lam)

  def ann = for {
    term <- non_ann
    _ <- reservedOp("::")
    ty <- readSigma
  } yield Ann(term, ty, term.id)

  def parens[T](p: => Parser[T]): Parser[T] = "(" ~> p <~ ")"

  def reservedOp(op: String): Parser[String] = if (reservedOps.contains(op)) op else failure("not a valid operator: " + op)

  def reserved(kw: String): Parser[String] = if (keywords.contains(kw)) kw else failure("not a valid keyword: " + kw)

  def let: Parser[Term] = log("let" ~> (identifier ~ args <~ "=") ~ term ^^ letExpr)("let")

  def app: Parser[Term] =
    rep1(atom).map{case fun::args =>
      args.foldLeft(fun)((f, a) => App(f, a, Raw(f.id.name + a.id.name)))
      case Nil => sys.error("app: empty list") //impossible
    }

  def lam: Parser[Term] = reservedOp("\\") ~> (ann_lam | ord_lam)

  def ord_lam: Parser[Term] = (rep(identifier) <~ reservedOp("->")) ~ term ^^ nestedLam

  def ann_lam: Parser[Term] = log(
    for {
      (vs, ex, ty) <- for {
        vs <- rep(identifier) <~ reservedOp("->")
        ex <- term
        _ <- reservedOp("::")
        ty <- readSigma
      } yield (vs, ex, ty)
      _ <- dot
      body <- term
    } yield nestedALam(vs, body, ty)
  )("ann_lam")

  private def nestedALam(vars: List[Ident], body: Term, ty: Sigma): ALam = vars.reverse match {
    case Nil => sys.error("nestedLam: empty list")
    case x::xs => xs.foldLeft(ALam(x, ty, body, body.id))((l, i) => ALam(i, ty, l, l.id))
  }

  def name: String => Raw = Raw(_)

  def optArgs: Option[List[Var]] => List[Var] = a => a match {
    case None => List[Var]()
    case Some(vars) => vars
  }

  def letExpr: Ident ~ List[Ident] ~ Term => Let = {case id ~ vars ~ t => vars match {
    case Nil => Let(id, t)
    case x::xs => Let(id, xs.foldLeft(Lam(x, t))((l, i) => Lam(i, l)))
  }}

  def nestedLam: List[Ident] ~ Term => Lam = {case vars ~ t => vars.reverse match {
    case Nil => sys.error("nestedLam: empty list")
    case x::xs => xs.foldLeft(Lam(x, t))((l, i) => Lam(i, l))
  }}


  def literalLit: Literal => Lit = l => l match {
    case IntLit(v) => Lit(l, Raw(v.toString))
    case DoubleLit(v) => Lit(l, Raw(v.toString))
    case CharLit(c) => Lit(l, Raw(c.toString))
    case StringLit(s) => Lit(l, Raw(s))
  }

  def dot: Parser[Char] = '.'

  def rfun: Parser[Rho] = for {
    arg <- atomSigma
    _ <- reservedOp("->")
    res <- readRho
  } yield Fun(arg, res)

  def readRho: Parser[Rho] = rfun | atomRho

  def atomRho: Parser[Rho] = tvar | tcon | parens(readRho)

  def readSigma: Parser[Sigma] = parens(readSigma) | sigma | readRho

  def atomSigma: Parser[Sigma] = parens(sigma) | atomRho

  def sigma: Parser[Sigma] = for {
    _ <- reserved("forall")
    tvs <- readTvs
    _ <- dot
    rho <- readRho
  } yield ForAll(tvs.map(id => BoundTv(id.name)), rho)

  def readTvs: Parser[List[Ident]] = rep(identifier)

  def readTau: Parser[Tau] = tfun | atomTau

  def tfun: Parser[Tau] = identifier.map(i => BoundTv(i.name))

  def fail[T](msg: String): Parser[T] =  Parser[T]{ in => Failure(msg, in) }

  def tvar: Parser[Tau] = identifier.flatMap(i => {
     val res = if (isBasicType(i)) failure("") else Parser(in => Success((), in))
     res.map(_ => BoundTv(i.name))
    })

  def isBasicType(i: Ident): Boolean =
    i.name == "Int" || i.name == "Double" || i.name == "Bool" || i.name == "String"  || i.name == "Char"

  def tcon: Parser[Tau] = identifier.map(i => i.name match {
    case "Int" => IntT
    case "Double" => DoubleT
    case "String" => StringT
    case "Char" => CharT
    case o => sys.error("tcon: not a basic type " + o)
  })

  def atomTau: Parser[Tau] = tvar | tcon | parens(readTau)
}
