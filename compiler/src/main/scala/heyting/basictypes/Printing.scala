package heyting
package basictypes

import prettyprint.Doc
import prettyprint.Docs._
import basictypes.Types._
import ast._


trait Output[A] {
  def ppr(a: A): Doc
}

object Printing {

  implicit val termOutput: Output[Term] = new Output[Term] {
    def ppr(t: Term) = t match {
      case Var(n) => pprName(n)
      case Lit(l) => pprLit(l)
      case App(l, r) => pprApp(App(l, r))
      case Lam(v, e) => sep(IndexedSeq(char('\\') <> pprName(v) <> text("."), termOutput.ppr(e)))
      case ALam(v, t, e) => sep(IndexedSeq(char('\\') <> parens(pprName(v) <> dcolon <> typeOutput.ppr(t))))
      case Let(v, e) => sep(IndexedSeq(text("let {"), nest(2, pprName(v) <+> char(eqls) <+> termOutput.ppr(e) <+> char('}'))))
      case Ann(e, ty) => pprParentTerm(e) <+> dcolon <+> pprParendType(ty)
    }
  }

  def docToString(doc: Doc): String = render(doc)


  def pprName(i: Ident): Doc = text(i.name)

  def pprLit(l: Literal): Doc = l match {
    case IntLit(i) => int(i)
    case DoubleLit(d) => double(d)
    case CharLit(c) => char(c)
    case StringLit(s) => text(s)
    case IdentLit(i) => text(i.name)
  }

  def pprApp(e: Term): Doc = {
    def go(t: Term, es: IndexedSeq[Term]): Doc = t match {
        case App(e1, e2) => go(e1, e2 +: es)
        case e1 => pprParentTerm(e1) <+> sep(es.map(pprParentTerm))
    }
    go(e, IndexedSeq())
  }

  def pprParentTerm(e: Term): Doc =
     if (e.atomicTerm) termOutput.ppr(e)
     else parens(termOutput.ppr(e))

  implicit val typeOutput: Output[Type] = new Output[Type] {
    def ppr(a: Type) = ppr_type(a)
  }

  def pprType(p: Precedence, ty: Type) : Doc = {
    if (p >= precType(ty)) parens(ppr_type(ty))
    else ppr_type(ty)
  }

  def ppr_type(ty: Type): Doc = ty match {
    case ForAll(ns, ty) => sep(Vector(text("forall") <+> hsep(ns.map(typeOutput.ppr)) <> dot, typeOutput.ppr(ty)))
    case Fun(arg, res) => sep(Vector(pprType(arrPrec, arg) <+> text("->"), pprType(arrPrec-1, res)))
    case tc: TyCon => ppr_tc(tc)
    case tv: TyVar => ppr_tv(tv)
    case MetaTv(u, tv) => ppr_tr(tv)
  }

  def precType(ty: Type): Precedence = ty match {
    case ForAll(_, _) => topPrec
    case Fun(_, _) => arrPrec
    case _ => atomicPrec
  }

  def pprParendType(ty: Type): Doc = pprType(tcPrec, ty)

  def ppr_tr(tr: TyRef): Doc = tr.read match {
    case Some(tau) => text("tau: " + tau)
    case None => empty
  }

  def ppr_tv(tv: TyVar): Doc = tv match {
    case BoundTv(n) => text("boundTv: " + n)
    case SkolemTv(n, _) => text("skolemTv: " + n)
  }

  def ppr_tc(tc: TyCon): Doc = tc match {
    case IntT => text("int")
    case DoubleT => text("double")
    case StringT => text("string")
    case BooleanT => text("boolean")
    case FloatT => text("float")
  }

  type Precedence = Int
  val topPrec = 0
  val arrPrec = 1
  val tcPrec = 2
  val atomicPrec = 3

}
