package heyting
package parser

import heyting.ast.Term
import compilation.{VerboseOptions, CompilationUnit}
import reports.ConsoleReport
import basictypes.{Offset, Positioned}

/**
 * User: arjan
 */
abstract class Parsing extends TermGrammar {
  def unit: CompilationUnit
  def source: SourceFile = unit.source
  def fileName: String = source.name

  def readTerms: ParseResult[List[Term]] = parseAll(terms, source.content)

  def position[T](p: => Parser[T]): Parser[Positioned[T]] = Parser { in => {
    val off = in.offset
    val line = in.pos.line
      p(in) match {
        case Success(t, in1) => Success(Positioned(fileName, Offset(line, in1.pos.line, off, in1.offset), t), in1)
        case ns: NoSuccess => ns
      }
    }
  }

  def parse: ParseResult[Term] = parseAll(term, source.content)

  def pparse: ParseResult[Positioned[Term]] = parseAll(position(term), source.content)
}

object Parsing {
  def stringParse(s: String) = new Parsing {
    def unit = new CompilationUnit(new VirtualSourceFile(s), ConsoleReport, VerboseOptions)
  }.parse
}