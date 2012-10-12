package heyting
package parser

import annotation.switch

import Tokens._
import collection.mutable.ListBuffer
import collection.immutable.HashMap

object Lexer {
  private[parser] final val kwTokenMap: Map[String, Int] = HashMap(
    "let" -> LET,
    "if" -> IF,
    "else" -> ELSE,
    "module" -> MODULE,
    "data" -> DATA,
    "where" -> WHERE,
    "in" -> IN
  )

  private[parser] final val kwHashTokenMap: Map[Int, Int] =
    kwTokenMap.map{case (a, b) => a.hashCode() -> b}

  def isKeyword(s: String): Boolean = kwHashTokenMap.contains(s.hashCode)

  def isKeyword(i: Int): Boolean = kwHashTokenMap.contains(i)
}

class Lexer(source: SourceFile) {
  private val content: Array[Char] = source.content
  private val length = content.length
  private val buf: StringBuilder = new StringBuilder
  var offset: Int = 0
  var lineNo: Int = 1
  var ch: Char = Char.MinValue
  var token: Int = EMPTY
  private var name: Name = _
  var prevToken: Token = _
  var currToken: Token = _
  final val SU = '\u001A' //substitute char, indicates EOF

  consume

  private final def reset = {
    ch = Char.MinValue
    token = EMPTY
    buf.clear()
    currToken = null
    offset = 0
    consume
  }

  private[parser] final def nextToken() {
    fetchToken()
  }

  private[parser] final def fetchToken() {
    (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
         'F' | 'G' | 'H' | 'I' | 'J' |
         'K' | 'L' | 'M' | 'N' | 'O' |
         'P' | 'Q' | 'R' | 'S' | 'T' |
         'U' | 'V' | 'W' | 'X' | 'Y' |
         'Z' | '$' | '_' |
         'a' | 'b' | 'c' | 'd' | 'e' |
         'f' | 'g' | 'h' | 'i' | 'j' |
         'k' | 'l' | 'm' | 'n' | 'o' |
         'p' | 'q' | 'r' | 's' | 't' |
         'u' | 'v' | 'w' | 'x' | 'y' |
         'z' =>
      putChar(ch)
      consume
      identifier
    case ' ' | Chars.CR =>
//      putChar(ch)
      consume
      fetchToken()
    case '"' =>
      putChar(ch)
      consume
      string
//    case Chars.CR =>
//      consume
//      fetchToken()
    case Chars.LF =>
      consume
      lineNo += 1
      fetchToken()
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '\\' =>
      putChar(ch)
      consume
      operatorRest
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
      putChar(ch)
      consume
      number
    case '[' => consume; token = LBRACKET; charToken
    case ']' => consume; token = RBRACKET; charToken
    case '(' => consume; token = LPAREN; charToken
    case ')' => consume; token = RPAREN; charToken
    case '{' => consume; token = LBRACE; charToken
    case '}' => consume; token = RBRACE; charToken
    case SU => token = EOF; charToken
    case _ => sys.error("could not tokenize character " + ch)
  }}

  private final def charToken {
    currToken = mkToken(token, ch.toString)
  }

  private final def identifier: Unit = (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
         'F' | 'G' | 'H' | 'I' | 'J' |
         'K' | 'L' | 'M' | 'N' | 'O' |
         'P' | 'Q' | 'R' | 'S' | 'T' |
         'U' | 'V' | 'W' | 'X' | 'Y' |
         'Z' | '$' |
         'a' | 'b' | 'c' | 'd' | 'e' |
         'f' | 'g' | 'h' | 'i' | 'j' |
         'k' | 'l' | 'm' | 'n' | 'o' |
         'p' | 'q' | 'r' | 's' | 't' |
         'u' | 'v' | 'w' | 'x' | 'y' |
         'z' |
         '0' | '1' | '2' | '3' | '4' |
         '5' | '6' | '7' | '8' | '9' | '_' =>
      putChar(ch)
      consume
      identifier
    case Chars.SU => // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
    case _ =>
      if (Character.isUnicodeIdentifierPart(ch)) {
        putChar(ch)
        consume
        identifier
      } else identToken()
  }

  private final def operatorRest: Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '\\' =>
      putChar(ch); consume; operatorRest
    case _ =>
      identToken()
  }

  private final def numberToken: Unit =  {
    setToken(mkToken(token, buf.toString()))
  }

  private final def number: Unit = (ch: @switch) match {
    case '0' | '1' | '2' | '3' | '4' |
         '5' | '6' | '7' | '8' | '9' =>
      putChar(ch); consume; number
    case '.' => putChar(ch); token = DOUBLELIT; consume; number
    case 'd' | 'D' => putChar(ch); token = DOUBLELIT; numberToken
    case 'f' | 'F' => putChar(ch); token = FLOATLIT; numberToken
    case _ => token = INTLIT; numberToken
  }


  private final def whitespace: Unit = (ch: @switch) match {
    case ' ' => putChar(ch); consume; whitespace
    case _ => setToken(mkToken(WHITESPACE, buf.toString))
  }

  private final def string: Unit = (ch: @switch) match {
    case '"' => {
      setToken(mkToken(STRINGLIT, buf.toString))
    }
    case _ => {
      putChar(ch)
      consume
      string
    }
  }

  private final def setToken(t: Token) = {
    prevToken = currToken
    currToken = t
    buf.clear
    token = currToken.code
  }

  private final def mkToken(token: Int, value: String) = Token(token, value, lineNo, offset, offset)

  private final def identToken(idtoken: Int = IDENTIFIER) {
    setToken(newIdentifier(buf.toString()))
  }

  private final def newIdentifier(chars: String): Token = {
    chars match {
      case "==" => mkToken(EQ, chars)
      case "=" => mkToken(ASSIGNMENT, chars)
      case "!=" => mkToken(NE, chars)
      case "<" => mkToken(LT, chars)
      case ">" => mkToken(GT, chars)
      case "<-" => mkToken(LARROW, chars)
      case "->" => mkToken(RARROW, chars)
      case "true" => mkToken(TRUE, chars)
      case "false" => mkToken(FALSE, chars)
      case _ => {
        val kw = keyword(chars)
        if (kw.isDefined) mkToken(kw.get, chars)
        else mkToken(IDENTIFIER, chars)
      }
    }
  }

  private final def keyword(s: String): Option[Int] = Lexer.kwTokenMap.get(s)

  final def isKeyword(i: Int): Boolean = Lexer.isKeyword(i)

  private final def consume: Unit =
    if (offset >= length) ch = SU
    else {
      ch = content(offset)
      offset += 1
    }

  private final def putChar(c: Char): Unit = {
    println("putting char " + c)
    buf.append(c)
  }

  final def tokenize: List[Token] = {
    val lb = ListBuffer[Token]()
    while (token != EOF) {
      fetchToken
      if (token != EOF) lb += currToken
    }
    lb.toList
  }

}
