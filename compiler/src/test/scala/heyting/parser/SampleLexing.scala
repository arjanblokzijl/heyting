package heyting
package parser

/**
 * User: arjan
 */
object SampleLexing extends App {
  val source = VirtualSourceFile("abcde  defghi")
  val source2 = VirtualSourceFile("let a = 1")
  val tokens = new Lexer(source2).tokenize
  println("got tokens: " + tokens)
}
