package heyting
package parser

import compilation.CompilationUnit
import reports.ConsoleReport


/**
 * User: arjan
 */
object ParsingSamples extends App {
  val s = VirtualSourceFile("let a = 1")
  val l = new Lexer(s)
//  println("tokens " + l.tokenize)

  val parser = new Parser {
    def in = l
    def source = s
    def unit = new CompilationUnit(s, ConsoleReport)
  }

  val tree = parser.parse
  println("tree is " + tree)

}
