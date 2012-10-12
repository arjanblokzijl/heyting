package heyting
package parser
import java.io.{File => JFile, InputStream, IOException, FileInputStream}
import java.nio.ByteBuffer

abstract class SourceFile {
  def content : Array[Char]

  def size: Int

  def compilationUnit: Boolean

  def name: String
}

class PhysicalSourceFile(file: JFile) extends SourceFile {
  val size = file.length.toInt

  def compilationUnit = true

  val content =
    new String(toByteArray(new FileInputStream(file))).toCharArray


  def name = file.getCanonicalPath

  /** Returns contents of file (if applicable) in a byte array.
   */
  @throws(classOf[IOException])
  def toByteArray(input: InputStream): Array[Byte] = {
    val in = input
    var rest = size
    val arr = new Array[Byte](rest)
    while (rest > 0) {
      val res = in.read(arr, arr.length - rest, rest)
      if (res == -1)
        throw new IOException("read error")
      rest -= res
    }
    in.close()
    arr
  }
}

case class VirtualSourceFile(s: String) extends SourceFile {
  val content = s.toCharArray

  val size = s.length

  def compilationUnit = false

  def name = "<Virtual File>"
}