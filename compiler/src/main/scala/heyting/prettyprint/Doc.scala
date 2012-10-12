package heyting.prettyprint

import Docs._

sealed trait Mode
case object PageMode extends Mode
case object ZigZagMode extends Mode
case object LeftMode extends Mode
case object OneLineMode extends Mode


sealed trait Doc {
  def beside[A](g: Boolean, rd: RDoc[A]): RDoc[A] = (this, rd) match {
    case (NoDoc, _) => NoDoc
    case (Union(p1, p2), q) => p1.beside(g, union_(q, p2.beside(g, q)))
    case (Empty, q) => q
    case (Nest(k, p), q) => nest_(k, p.beside(g, q))
    case (p@Beside(p1, g1, q1), q2) => {
      if (g1 == g) p1.beside(g1, q1.beside(g, q2))
      else p.reduceDoc.beside(g, q2)
    }
    case (p@Above(_, _, _), q) => p.reduceDoc.beside(g, q)
    case (NilAbove(p), q) => nilAbove_(p.beside(g, q))
    case (TextBeside(s, sl, p), q) => {
      val rest = p match {
        case Empty => nilBeside(g, q)
        case _ => p.beside(g, q)
      }
      textBeside_(s, sl, rest)
    }
  }

  def reduceDoc[A]: RDoc[A] = this match {
    case Beside(p, g, q) => p.beside(g, q.reduceDoc)
    case Above(p, g, q) => p.above(g, q.reduceDoc)
    case p => p
  }

  def above_[A](g: Boolean, rd: RDoc[A]): RDoc[A] = (this, rd) match {
    case (p, Empty) => p
    case (Empty, q) => q
    case (p, q) => Above(p, g, q)
  }

  def above[A](g: Boolean, rd: RDoc[A]): RDoc[A] = (this, rd) match {
    case (Above(p, g1, q1), q2) => p.above(g1, q1.above(g, q2))
    case (p@Beside(_,_,_), q) => aboveNest(p.reduceDoc, g, 0, q.reduceDoc)
    case (p, q) => aboveNest(p, g, 0, q.reduceDoc)
  }

  def beside_(g: Boolean, d: Doc) = (this, d) match {
    case (p, Empty) => p
    case (Empty, q) => q
    case (p, q) => Beside(p, g, q)
  }

  def <>(q: Doc): Doc = beside_(false, q)

  def <+>(q: Doc): Doc = beside_(true, q)
}

case object Empty extends Doc
case object NoDoc extends Doc
case class NilAbove(d: Doc) extends Doc
case class TextBeside(t: TextDetails, i: Int, d: Doc) extends Doc
case class Nest(i: Int, d: Doc) extends Doc
case class Union(d1: Doc, d2: Doc) extends Doc
case class Beside(d1: Doc, b: Boolean, d2: Doc) extends Doc
case class Above(d1: Doc, b: Boolean, d2: Doc) extends Doc


sealed trait TextDetails
case class Chr(c: Char) extends TextDetails
case class Str(s: String) extends TextDetails

object Docs {

  case class Style(mode: Mode, lineLength: Int, ribbonsPerLine: Float)

  val style: Style = Style(PageMode, 100, 1.5f)

  type RDoc[A] = Doc

  def text(s: String): Doc = sizedText(s.length, s)

  def sizedText(i: Int, s: String): Doc = textBeside_(Str(s), i, Empty)

  def char(c: Char): Doc = textBeside_(Chr(c), 1, Empty)

  def empty: Doc = Empty

  def zeroWidthText(s: String): Doc = sizedText(0, s)

  def indent(i: Int): String = replicate(i, ' ')

  final val semi: Char = ';'
  final val comma: Char = ','
  final val colon: Char = ':'
  final val space: Char = ' '
  final val eqls: Char = '='
  final val lparen: Char = '('
  final val rparen: Char = ')'
  final val lbrack: Char = '['
  final val rbrack: Char = ']'
  final val lbrace: Char = '{'
  final val rbrace: Char = '}'
  val dcolon: Doc = text("::")
  val dot: Doc = char('.')
  def quotes(p: Doc): Doc = char('\'') <> p <> char('\'')
  val space_text: TextDetails = Chr(' ')
  val nl_text: TextDetails = Chr('\n')

  def parens(p: Doc): Doc = char(lparen) <> p <> char(rparen)
  def brackets(p: Doc): Doc = char(lbrack) <> p <> char(rbrack)
  def braces(p: Doc): Doc = char(lbrace) <> p <> char(rbrace)


  def int(i: Int): Doc = text(i.toString)
  def float(f: Float): Doc = text(f.toString)
  def double(d: Double): Doc = text(d.toString)

  def hcat(ys: IndexedSeq[Doc]): Doc = reduceAB(ys.foldLeft(empty)(beside_1(false, _, _)))

  def hsep(ys: IndexedSeq[Doc]): Doc = reduceAB(ys.foldLeft(empty)(beside_1(true, _, _)))

  def vcat(ys: IndexedSeq[Doc]): Doc = reduceAB(ys.foldLeft(empty)(above_1(false, _, _)))

  def sep(ys: IndexedSeq[Doc]): Doc = sepX(true, ys)

  def sepX(g: Boolean, ys: IndexedSeq[Doc]): Doc =
    if (ys.isEmpty) empty
    else sep1(g, ys.head.reduceDoc, 0, ys.tail)

  def sep1[A](g: Boolean, d: RDoc[A], k: Int, ys: IndexedSeq[Doc]): RDoc[A] = d match {
    case NoDoc => NoDoc
    case Union(p, q) => union_(sep1(g, p, k, ys), aboveNest(q, false, k, vcat(ys).reduceDoc))
    case Empty => mkNest(k, sepX(g, ys))
    case Nest(n, p) => nest_(n, sep1(g, p, k - n, ys))
    case NilAbove(p) => nilAbove_(aboveNest(p, false, k, vcat(ys).reduceDoc))
    case TextBeside(s, sl, p) => textBeside_(s, sl, sepNB(g, p, k - sl, ys))
    case d: Above => sys.error("sep1 Above")
    case d: Beside => sys.error("sep1 Beside")
  }


  def beside_1(g: Boolean, d1: Doc, d2: Doc): Doc = (d1, d2) match {
    case (p, Empty) => p
    case (p, q) => Beside(p, g, q)
  }

  def above_1(g: Boolean, d1: Doc, d2: Doc): Doc = (d1, d2) match {
    case (p, Empty) => p
    case (p, q) => Above(p, g, q)
  }

  def reduceAB(d: Doc): Doc = d match {
    case Above(Empty, _, q) => q
    case Beside(Empty, _, q) => q
    case d => d
  }

  def sepNB(g: Boolean, d: Doc, k: Int, ys: IndexedSeq[Doc]): Doc = d match {
    case Nest(_, p) => sepNB(g, p, k, ys)
    case Empty => {
      val rest = if (g) hsep(ys) else hcat(ys)
      mkUnion(oneLiner(nilBeside(g, rest.reduceDoc)), nilAboveNest(false, k, vcat(ys).reduceDoc))
    }
    case p => sep1(g, p, k, ys)
  }

  def oneLiner(d: Doc): Doc = d match {
    case NoDoc => NoDoc
    case Empty => Empty
    case NilAbove(_) => NoDoc
    case TextBeside(s, sl, p) => textBeside_(s, sl, oneLiner(p))
    case Nest(k, p) => nest_(k, oneLiner(p))
    case Union(p, _) => oneLiner(p)
    case d: Above => sys.error("oneLiner Above")
    case d: Beside => sys.error("oneLiner Beside")
  }

  def mkUnion(d1: Doc, d2: Doc): Doc = (d1, d2) match {
    case (Empty, _) => Empty
    case (p, q) => union_(p, q)
  }

  def union_[A](p: RDoc[A], q: RDoc[A]): RDoc[A] = Union(p, q)

  def nest_[A](k: Int, p: RDoc[A]): RDoc[A] = Nest(k, p)

  def nest(k: Int, d: Doc): Doc = mkNest(k, d.reduceDoc)

  def mkNest(k: Int, d: Doc): Doc = d match {
    case Nest(k1, p) => mkNest(k + k1, p)
    case NoDoc => NoDoc
    case Empty => Empty
    case p => if (k == 0) p else nest_(k, p)
  }

  def textBeside_[A](s: TextDetails, sl: Int, p: RDoc[A]): RDoc[A] = TextBeside(s, sl, p)

  def nilBeside[A](g: Boolean, d: RDoc[A]): RDoc[A] = d match {
    case Empty => Empty
    case Nest(_, p) => nilBeside(g, p)
    case p => {
      if (g) textBeside_(space_text, 1, p)
      else p
    }
  }

  def aboveNest[A](l: RDoc[A], g: Boolean, k: Int, r: RDoc[A]): RDoc[A] = (l, r) match {
    case (NoDoc, _) => NoDoc
    case (Union(p1, p2), q) => aboveNest(p1, g, k, union_(q, aboveNest(p2, g, k, q)))
    case (Empty, q) => mkNest(k, q)
    case (Nest(k1, p), q) => nest_(k1, aboveNest(p, g, (k - k1), q))
    case (NilAbove(p), q) => nilAbove_(aboveNest(p, g, k, q))
    case (TextBeside(s, s1, p), q) => {
      val k1 = k - s1
      val rest = p match {
        case Empty => nilAboveNest(g, k1, q)
        case _ => aboveNest(p, g, k1, q)
      }
      textBeside_(s, s1, rest)
    }
    case (Above(_, _, _), _) => sys.error("aboveNest Above")
    case (Beside(_, _, _), _) => sys.error("aboveNest Beside")
  }

  def nilAboveNest[A](g: Boolean, k: Int, r: RDoc[A]): RDoc[A] = r match {
    case Empty => Empty
    case Nest(k1, q) => nilAboveNest(g, k + k1, q)
    case q => if (!g && k > 0) textBeside_(Str(indent(k)), k, q)
              else nilAbove_(mkNest(k, q))
  }

  def nilAbove_[A](p: RDoc[A]): RDoc[A] = NilAbove(p)

  def replicate(n: Int, c: Char): String = (0 to n).map(_ => c).mkString


  def first(p: Doc, q: Doc): Doc = if (nonEmptySet(p)) p else q

  def nonEmptySet(d: Doc): Boolean = d match {
    case NoDoc => false
    case Union(_, _) => true
    case Empty => true
    case NilAbove(_) => true
    case TextBeside(_, _, p) => nonEmptySet(p)
    case Nest(_, p) => nonEmptySet(p)
    case a: Above => sys.error("nonEmptySet Above")
    case b: Beside => sys.error("nonEmptySet Beside")
  }

  def nicest(w: Int, r: Int, p: Doc, q: Doc): Doc = nicest1(w, r, 0, p, q)

  def nicest1(w: Int, r: Int, sl: Int, p: Doc, q: Doc): Doc =
    if (fits(math.min(w, r) - sl, p)) p
    else q

  def fits(n: Int, d: Doc): Boolean = if (n < 0) false else d match {
    case Empty => true
    case NoDoc => false
    case NilAbove(_) => true
    case TextBeside(_, sl, p) => fits(n - sl, p)
    case n: Nest => sys.error("fits Nest")
    case u: Union => sys.error("fits Union")
    case a: Above => sys.error("fits Above")
    case b: Beside => sys.error("fits Beside")
  }

  def best[A](w0: Int, r: Int, p0: RDoc[A]): RDoc[A] = {
    def get(w: Int, d: RDoc[A]): RDoc[A] = d match {
      case Empty => Empty
      case NoDoc => NoDoc
      case NilAbove(p) => nilAbove_(get(w, p))
      case TextBeside(s, sl, p) => textBeside_(s, sl, get1(w, sl, p))
      case Nest(k, p) => nest_(k, get(w - k, p))
      case Union(p, q) => nicest(w, r, get(w, p), get(w, q))
      case a: Above => sys.error("best get Above")
      case b: Beside => sys.error("best get Beside")
    }

    def get1(w: Int, sl: Int, d: RDoc[A]): RDoc[A] = d match {
      case Empty => Empty
      case NoDoc => NoDoc
      case NilAbove(p) => nilAbove_(get(w - sl, p))
      case TextBeside(t, tl, p) => textBeside_(t, tl, get1(w, sl + tl, p))
      case Nest(_, p) => get1(w, sl, p)
      case Union(p, q) => nicest1(w, r, sl, get1(w, sl, p), get1(w, sl, q))
      case a: Above => sys.error("best get1 Above")
      case b: Beside => sys.error("best get1 Beside")
    }
    get(w0, p0)
  }

  def render(doc: Doc): String =
    fullRender(style.mode, style.lineLength, style.ribbonsPerLine, txtPrinter, "", doc)

  def fullRender[A](m: Mode, lineLen: Int, ribbons: Float, txt: (TextDetails, A) => A, end: A, doc: Doc): A = m match {
    case OneLineMode => easy_display(space_text, (x, y) => y, txt, end, doc.reduceDoc)
    case LeftMode => easy_display(nl_text, first, txt, end, doc.reduceDoc)
    case m => {
      val bestLineLen = m match {
        case ZigZagMode => Int.MaxValue
        case _ => lineLen
      }
      val ribbonLen = math.round(lineLen / ribbons)
      val doc1 = best(bestLineLen, ribbonLen, doc.reduceDoc)
      display(m, lineLen, ribbonLen, txt, end, doc1)
    }
  }

  def txtPrinter(td: TextDetails, s: String): String = td match {
    case Chr(c) => c +: s
    case Str(s1) => s1 ++ s
  }

  def easy_display[A](nl_space_text: TextDetails, choose: (Doc, Doc) => Doc, txt: (TextDetails, A) => A, end: A, doc: Doc): A = {
    def lay(d: Doc): A = d match {
      case NoDoc => sys.error("easy_display: NoDoc")
      case Union(p, q) => lay(choose(p, q))
      case Nest(_, p) => lay(p)
      case Empty => end
      case NilAbove(p) => txt(nl_space_text, lay(p))
      case TextBeside(s, _, p) => txt(s, lay(p))
      case a: Above => sys.error("easy_display: Above")
      case b: Beside => sys.error("easy_display: Beside")
    }
    lay(doc)
  }

  def display[A](m: Mode, page_width: Int, ribbon_width: Int, txt: (TextDetails, A) => A, end: A, doc: Doc): A = {
    val gap_width = page_width - ribbon_width
    val shift = gap_width / 2
    def lay(k: Int, d: Doc): A = d match {
      case Nest(k1, p) => lay(k + k1, p)
      case Empty => end
      case NilAbove(p) => txt(nl_text, lay(k, p))
      case TextBeside(s, sl, p) => m match {
        case ZigZagMode =>
          if (k >= gap_width) txt(nl_text, txt(Str(replicate(shift, ' ')), txt(nl_text, (lay1(k - shift, s, sl, p)))))
          else if (k < 0) txt(nl_text, txt(Str(replicate(shift, '\\')), txt(nl_text, lay1(k - shift, s, sl, p))))
          else lay1(k + shift, s, sl, p)
        case _ => lay1(k, s, sl, p)
      }
      case Above(_, _, _) => sys.error("display lay Above")
      case Beside(_, _, _) => sys.error("display lay Beside")
      case NoDoc => sys.error("display lay NoDoc")
      case u: Union => sys.error("display lay Union")
    }

    def lay1(k: Int, s: TextDetails, sl: Int, p: Doc): A = {
      val r = k + sl
      txt(Str(indent(k)), txt(s, lay2(r, p)))
    }
    def lay2(k: Int, d: Doc): A = d match {
      case NilAbove(p) => txt(nl_text, lay(k, p))
      case TextBeside(s, sl, p) => txt(s, lay2(k + sl, p))
      case Nest(_, p) => lay2(k, p)
      case Empty => end
      case a: Above => sys.error("display lay2 Above")
      case b: Beside => sys.error("display lay2 Beside")
      case NoDoc => sys.error("display lay2 NoDoc")
      case u: Union => sys.error("display lay2 Union")
    }

    lay(0, doc)
  }
}