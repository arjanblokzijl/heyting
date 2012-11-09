package heyting
package compilation

/**
 * User: arjan
 */
trait Settings {
  def flags: Set[Flag]

  def hasFlag(f: Flag): Boolean = flags.contains(f)

  def trace: Boolean = hasFlag(TRACE)
  def debug: Boolean = trace || hasFlag(DEBUG)
  def info: Boolean = debug || hasFlag(INFO)
  def warn: Boolean = info || hasFlag(WARN)
  def error: Boolean = true
}

sealed trait Flag

object TRACET extends Flag
object TRACEEXP extends Flag
object TRACE extends Flag
object DEBUG extends Flag
object INFO extends Flag
object WARN extends Flag
object ERROR extends Flag

object Verbose extends Settings {
  val flags = Set(TRACET, TRACEEXP, TRACE)
}
object Silent extends Settings {
  val flags = Set[Flag](ERROR)
}