package heyting
package compilation

import parser.SourceFile
import reports.Reporting
import basictypes.Position

trait Compilation {
  def source: SourceFile
  def report: Reporting
  def settings: Settings
}

class CompilationUnit(val source: SourceFile, val report: Reporting, val settings: Settings) extends Compilation {
  def trace(msg: String) =
    if (settings.trace) report.trace(msg)

  def error(pos: Position, msg: String, b: Boolean) =
    report.error(pos, msg)

  def incompleteInputError(pos: Position, msg: String) =
    report.incompleteInputError(pos, msg)

}