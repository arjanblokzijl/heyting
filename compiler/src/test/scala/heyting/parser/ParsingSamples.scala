package heyting
package parser

import compilation.{VerboseOptions, StdOptions, CompilationUnit}
import reports.ConsoleReport


/**
 * User: arjan
 */
object ParsingSamples extends App {
  val s = VirtualSourceFile("let add a b = + a b")
  val l = new Lexer(s)

  val pp = new Parsing {
    def unit = new CompilationUnit(s, ConsoleReport, VerboseOptions)
  }

  val res = pp.parseString
  println("pp.parse is " + res)

}
