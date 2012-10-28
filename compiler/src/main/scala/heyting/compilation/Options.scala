package heyting
package compilation

/**
 * User: arjan
 */
trait Options {
  def flags: Seq[Flag]

  def hasFlag(f: Flag): Boolean = flags.contains(f)
}

sealed trait Flag

object TRACET extends Flag
object TRACEEXP extends Flag
object TRACE extends Flag

object VerboseOptions extends Options {
  val flags = IndexedSeq(TRACET, TRACEEXP)
}
object StdOptions extends Options {
  val flags = IndexedSeq()
}