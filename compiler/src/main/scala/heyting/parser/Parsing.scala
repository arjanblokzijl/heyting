package heyting
package parser

import heyting.ast.Term
import compilation.{VerboseOptions, CompilationUnit}
import reports.ConsoleReport

/**
 * User: arjan
 */
abstract class Parsing extends TermGrammar {
  def unit: CompilationUnit
  def source: SourceFile = unit.source

  def parse: ParseResult[Term] = parseAll(readTerm, source.content)
}

object Parsing {
  def stringParse(s: String) = new Parsing {
    def unit = new CompilationUnit(new VirtualSourceFile(s), ConsoleReport, VerboseOptions)
  }.parse
}