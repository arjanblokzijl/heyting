package heyting
package parser

import heyting.ast.Term
import compilation.{Verbose, CompilationUnit}
import reports.ConsoleReport
import basictypes.{Offset, Pos}

/**
 * User: arjan
 */
abstract class Parsing extends TermGrammar {
  def unit: CompilationUnit
  def source: SourceFile = unit.source
  def fileName: String = source.name

  val report = unit.report
  def readTerms: ParseResult[List[Term]] = parseAll(terms, source.content)

  def position[T](p: => Parser[T]): Parser[Pos[T]] = Parser { in => {
    val off = in.offset
    val line = in.pos.line
      p(in) match {
        case Success(t, in1) => Success(Pos(fileName, Offset(line, in1.pos.line, off, in1.offset), t), in1)
        case ns: NoSuccess => ns
      }
    }
  }

  override def log[T](p: => Parser[T])(name: String) =
    Parser{ in =>
      if (unit.settings.trace) {
       report.trace("trying "+ name +" at "+ in)
       val r = p(in)
        report.trace(name +" --> "+ r)
       r
     } else p(in)

  }

  def parse: ParseResult[Term] = parseAll(term, source.content)

  def pparse: ParseResult[Pos[Term]] = parseAll(position(term), source.content)
}

object Parsing {
  def stringParse(s: String) = new Parsing {
    def unit = new CompilationUnit(new VirtualSourceFile(s), new ConsoleReport(Verbose), Verbose)
  }.parse
}