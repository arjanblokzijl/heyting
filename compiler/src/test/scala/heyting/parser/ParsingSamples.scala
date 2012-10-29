package heyting
package parser

import compilation.{VerboseOptions, StdOptions, CompilationUnit}
import reports.ConsoleReport


/**
 * User: arjan
 */
object ParsingSamples extends App {
  val s = VirtualSourceFile("let add a b = (+ a b)")
  val s2 = VirtualSourceFile("(add a b)")

  val pp = new Parsing {
    def unit = new CompilationUnit(s, ConsoleReport, VerboseOptions)
  }

  val res = pp.parse
  println("pp.parse is " + res)


}
