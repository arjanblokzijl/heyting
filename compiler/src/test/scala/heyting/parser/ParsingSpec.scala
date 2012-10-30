package heyting
package parser

import org.specs2.mutable.Specification
import compilation.{VerboseOptions, CompilationUnit}
import reports.ConsoleReport
import ast._
import basictypes._

/**
 * User: arjan
 */
class ParsingSpec extends Specification {
  val s = VirtualSourceFile("let add a b = (+ a b)")
  val s2 = VirtualSourceFile("(add a b)")

  def parse(expr: String) = new Parsing {
    def unit = new CompilationUnit(VirtualSourceFile(expr), ConsoleReport, VerboseOptions)
  }.parse

  "parsing" should {
    "let statement" in {
      val res: Term = parse("let a = 1").get
      res must be_==(Let(Raw("a"), Lit(IntLit(1), Raw("1"))))
    }
    "lam" in {
      val res = parse("\\a b -> a + b").get
      res must be_==(Lam(Raw("a"),Lam(Raw("b"),App(App(Var(Raw("a")),Var(Raw("+")),Raw("a+")),Var(Raw("b")),Raw("a+b")))))
    }
  }
}
