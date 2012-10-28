package heyting
package parser

import heyting.ast.Term
import compilation.CompilationUnit

/**
 * User: arjan
 */
abstract class Parsing extends TermGrammar {
  def unit: CompilationUnit
  def source: SourceFile = unit.source

  def parseString: ParseResult[Term] = parseAll(expr, source.content)
}
