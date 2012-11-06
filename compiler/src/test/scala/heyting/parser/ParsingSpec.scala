package heyting
package parser

import org.specs2.mutable.Specification
import compilation.{VerboseOptions, CompilationUnit}
import reports.ConsoleReport
import ast._
import basictypes._
import basictypes.Types.BoundTv

/**
 * User: arjan
 */
class ParsingSpec extends Specification {
  def parse(expr: String) = new Parsing {
    def unit = new CompilationUnit(VirtualSourceFile(expr), ConsoleReport, VerboseOptions)
  }.parse

  def pos(expr: String) = new Parsing {
    def unit = new CompilationUnit(VirtualSourceFile(expr), ConsoleReport, VerboseOptions)
  }.pparse

  def parseAll(expr: String) = new Parsing {
    def unit = new CompilationUnit(VirtualSourceFile(expr), ConsoleReport, VerboseOptions)
  }.readTerms

  "parsing" should {
    "let statement" in {
      val res: Term = parse("let a = 1").get
      res must be_==(Let(Raw("a"), Lit(IntLit(1), Raw("1"))))
    }
    "statement with position" in {
      val res = pos("let a = 1").get
      println("result is " + res)
      res must be_==(Positioned("<Virtual File>", Offset(1,1,0,9), Let(Raw("a"), Lit(IntLit(1), Raw("1")))))
    }
    "let statements newline" in {
      val res = parseAll("let a = 1\nlet b = 2").get
      res must be_==(List(Let(Raw("a"), Lit(IntLit(1), Raw("1"))), Let(Raw("b"), Lit(IntLit(2), Raw("2")))))
    }
    "let expr" in {
      val res: Term = parse("let add = \\a b -> a + b").get
      res must be_==(Let(Raw("add"),Lam(Raw("a"),Lam(Raw("b"),App(App(Var(Raw("a")),Var(Raw("+")),Raw("a+")),Var(Raw("b")),Raw("a+b"))))))
    }
    "lam" in {
      val res = parse("\\a b -> a + b").get
      res must be_==(Lam(Raw("a"),Lam(Raw("b"),App(App(Var(Raw("a")),Var(Raw("+")),Raw("a+")),Var(Raw("b")),Raw("a+b")))))
    }
    "ann" in {
      val res = parse("let add a b = (+ a b) :: forall a. a -> a -> a").get
      res must be_==(Let(Raw("add"),Lam(Raw("b"),Lam(Raw("a"),Ann(App(App(Var(Raw("+")),Var(Raw("a")),Raw("+a")),Var(Raw("b")),Raw("+ab")),
                        ForAll(List(BoundTv("a")),Fun(BoundTv("a"),Fun(BoundTv("a"),BoundTv("a")))),
                        Raw("+ab"))))))
    }
  }
}
