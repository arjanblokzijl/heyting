package heyting
package reports

import basictypes.Position
import compilation.Settings


/**
 * User: arjan
 */
trait Reporting {
  def incompleteInputError(pos: Position, msg: String)
  def settings: Settings

  def error(pos: Position, msg: String): Unit
  def trace(msg: String): Unit = if (settings.trace) tracef(msg)
  def tracef(msg: String): Unit
  def traceP(pos: Position, msg: String): Unit = if (settings.trace) tracePf(pos, msg)
  def tracePf(pos: Position, msg: String): Unit

  /**
   * Always logs the given message, without checking trace, debug, etc levels.
   */
  def logf(pos: Position, msg: String): Unit

  /**
   * Logs only if the level is greater than or equal to the currently set log level.
   */
  def log(level: Level, pos: Position, msg: String) = level match {
    case Trace if (settings.trace) => logf(pos, "trace: " + msg)
    case Debug if (settings.debug) => logf(pos, "debug: " + msg)
    case Info if (settings.debug) => logf(pos, "info: " + msg)
    case Warn if (settings.warn) => logf(pos, "warn: " + msg)
    case Error if (settings.debug) => logf(pos, "errror: " + msg)
  }
}

sealed trait Level {
  def id: Int
}
object Trace extends Level {
  val id: Int = 0
}
object Debug extends Level {
  val id: Int = 1
}
object Info extends Level {
  val id: Int = 2
}
object Warn extends Level {
  val id: Int = 3
}
object Error extends Level {
  val id: Int = 4
}

class ConsoleReport(val settings: Settings) extends Reporting {
  def error(pos: Position, msg: String) =
    println("error: " + msg + " at position " + pos)

  def logf(pos: Position, msg: String) = println(msg + " at " + pos)

  def incompleteInputError(pos: Position, msg: String) =
    println("incomplete input: " + msg + " at " + pos)
  def tracef(msg: String) = println(msg)
  def tracePf(pos: Position, msg: String) = println(msg + " at " + pos)
}
