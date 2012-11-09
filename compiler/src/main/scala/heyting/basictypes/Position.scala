package heyting
package basictypes

import parser.SourceFile

/**
 * User: arjan
 */
trait Position {
  def fileName: String
  def firstCol: Int
  def lastCol: Int
  def firstLine: Int
  def lastLine: Int

  def position: String = "Position: file %s, start %s, end %s" format (fileName, firstCol, lastCol)
}

case class Pos[T](fileName: String, offset: Offset, input: T) extends Position {
  def firstCol = offset.startOffset + 1
  def lastCol = offset.endOffset + 1
  def firstLine = offset.startLine
  def lastLine = offset.endLine
}

case class Offset(startLine: Int, endLine: Int, startOffset: Int, endOffset: Int)
