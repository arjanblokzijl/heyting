package heyting
package basictypes

import parser.{Token, SourceFile}

/**
 * User: arjan
 */
trait Position {
  def first: Token
  def last: Token
  def fileName: String

  override def toString = "line: " + first.line + " offset: " + first.offset
}

case class SrcSpan(source: SourceFile, first: Token, last: Token) extends Position {
  def fileName = source.name

  def startLine: Int = first.line

  def endLine: Int = last.line
}
