package heyting
package parser

/**
 * User: arjan
 */
trait Chars {
  final val EOF = -1.toChar
  final val SU = '\u001A'
  final val CR = '\u000D'
  final val LF = '\u000A'
}

object Chars extends Chars