package heyting
package reports

import basictypes.Position


/**
 * User: arjan
 */
trait Reporting {
  def incompleteInputError(pos: Position, msg: String)

  def error(pos: Position, msg: String): Unit
  def trace(msg: String): Unit
  def traceP(pos: Position, msg: String)
}

object ConsoleReport extends Reporting {
  def error(pos: Position, msg: String) =
    println("error: " + msg + " at position " + pos)

  def incompleteInputError(pos: Position, msg: String) =
    println("incomplete input: " + msg + " at position " + pos)

  def trace(msg: String) = println(msg)

  def traceP(pos: Position, msg: String) = println(msg + " at postition " + pos)
}
