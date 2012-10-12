package heyting
package compilation

import parser.SourceFile
import reports.Reporting
import basictypes.Position

class CompilationUnit(val source: SourceFile, val reporter: Reporting) {
  def error(pos: Position, msg: String, b: Boolean) =
    reporter.error(pos, msg)

  def incompleteInputError(pos: Position, msg: String) =
    reporter.incompleteInputError(pos, msg)

}

object Compilation
