package heyting
package compilation

import parser.SourceFile
import reports.Reporting
import basictypes.Position

trait Compilation {
  def source: SourceFile
  def report: Reporting
  def options: Options
}

class CompilationUnit(val source: SourceFile, val report: Reporting, val options: Options) extends Compilation {
  def trace(msg: String) =
    if (options.hasFlag(TRACE)) report.trace(msg)

  def error(pos: Position, msg: String, b: Boolean) =
    report.error(pos, msg)

  def incompleteInputError(pos: Position, msg: String) =
    report.incompleteInputError(pos, msg)

}

object Compilation
