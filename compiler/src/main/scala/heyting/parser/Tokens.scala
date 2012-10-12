package heyting
package parser

import annotation.switch

object Tokens {
  final val EMPTY = -3
  final val UNDEF = -2
  final val ERROR = -1
  final val EOF = 0
  /** literals */
  final val CHARLIT = 1
  final val INTLIT = 2
  final val LONGLIT = 3
  final val FLOATLIT = 4
  final val DOUBLELIT = 5
  final val STRINGLIT = 6
  final val WHITESPACE = 7
  final val TRUE = 8
  final val FALSE = 9
  final val IDENTIFIER = 10
  //operators
  final val PLUS = 11
  final val MINUS = 12
  final val MULT = 13
  final val DIV = 14
  final val MOD = 15
  final val EQ = 16
  final val NE = 17
  final val GE = 18
  final val LE = 19
  final val LT = 20
  final val GT = 21
  final val BOOLOP = 22
  final val LARROW = 23
  final val RARROW = 24
  final val LPAREN = 25
  final val RPAREN = 26
  final val LBRACKET = 27
  final val RBRACKET = 28
  final val LBRACE = 29
  final val RBRACE = 30
  final val ASSIGNMENT = 31
  final val FATRARROW = 32


  //keywords
  final val LET = 50
  final val IF = 51
  final val ELSE = 52
  final val MODULE = 53
  final val DATA = 54
  final val WHERE = 55
  final val IN = 56

  final val kwStart = LET
  final val kwEnd = IN

  final val COMMA = 70
  final val NEWLINE = 71

  def token2string(token: Int): String = (token: @switch) match {
    case EMPTY => "empty"
    case UNDEF => "undefined"
    case ERROR => "error"
    case EOF => "eof"
    case CHARLIT => "character literal"
    case INTLIT => "integer literal"
    case LONGLIT => "long literal"
    case FLOATLIT => "float literal"
    case DOUBLELIT => "double literal"
    case STRINGLIT => "string literal"
    case WHITESPACE => "whitespace"
    case LBRACKET => "'['"
    case RBRACKET => "']'"
    case IF => "if"
    case ELSE => "else"
    case COMMA => ","
    case LET => "let"
    case IDENTIFIER => "identifier"
    case LARROW => "left arrow"
    case RARROW => "right arrow"
    case PLUS => "plus"
    case MINUS => "minus"
    case MULT => "multiplier"
    case DIV => "division"
    case MOD => "modulo"
    case EQ => "equals"
    case NE => "not equals"
    case GE => "greater than or equals"
    case LE => "less than or equals"
    case LT => "less than"
    case GT => "greater than"
    case ASSIGNMENT => "assignment"
    case MODULE => "module"
    case BOOLOP => "boolean operator"
    case NEWLINE => "newline"
    case TRUE => "true"
    case FALSE => "false"
  }
}

import Tokens._
case class Token(code: Int, value: String, line: Int, indent: Int, offset: Int) {
  override def toString = "<" + token2string(code) + ": " + value + ">"
}
