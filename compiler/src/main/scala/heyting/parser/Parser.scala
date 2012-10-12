package heyting
package parser

import collection.immutable.HashMap
import ast._
import Tokens._
import compilation.CompilationUnit
import scala.annotation._
import ast.Let
import basictypes.{SrcSpan, Position, RawName}
import basictypes.Ident._

/**
 * User: arjan
 */
trait Parser {
  def in: Lexer
  def source: SourceFile
  def unit: CompilationUnit
  def parse: Tree = {
//    if (source.compilationUnit) compilationUnit()
//    else scriptUnit()
    scriptUnit()
  }

  final def isLiteralToken(token: Int) = token match {
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
         STRINGLIT | TRUE | FALSE => true
    case _  => false
  }

  final def isLiteral: Boolean = isLiteralToken(in.token)

  final def isExprIntroToken(token: Int): Boolean = isLiteralToken(token) || (token match {
    case IDENTIFIER | IF | LPAREN | LBRACE  => true
    case _ => false
  })

  final def isExprIntro: Boolean = isExprIntroToken(in.token)

  final def isLet: Boolean = {
    println("isLet: in.token is " + in.token)
    in.token match {
      case LET => true
      case _ => false
    }
  }



  /** Consume one token of the specified type, or signal an error if it is not there. */
   final def accept(token: Int): Int = {
     in.nextToken()
     val offset = in.offset
     if (in.token != token) {
       syntaxErrorOrIncomplete(expectedMsg(token), false)
       skip(token)
     }
     if (in.token == token) in.fetchToken()
     offset
   }

  final def skip(targetToken: Int) {
    var nparens = 0
    var nbraces = 0
    while (true) {
      in.token match {
        case EOF =>
          return
        case NEWLINE =>
          if (nparens == 0 && nbraces == 0) return
        case RPAREN =>
          nparens -= 1
        case RBRACE =>
          if (nbraces == 0) return
          nbraces -= 1
        case LPAREN =>
          nparens += 1
        case LBRACE =>
          nbraces += 1
        case _ =>
      }
      if (targetToken == in.token && nparens == 0 && nbraces == 0) return
      in.nextToken()
    }
  }

  def syntaxError(offset: Int, msg: String, skip: Boolean) = unit.error(o2p(offset), msg, skip)

  def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean) {
    if (in.token == EOF)
      incompleteInputError(msg)
    else
      syntaxError(in.offset, msg, skipIt)
  }

  def incompleteInputError(msg: String) {
    val offset = source.content.length - 1
    unit.incompleteInputError(o2p(offset), msg)
  }

  def o2p(offset: Int): Position = new SrcSpan(source, in.currToken, in.currToken)

  def expectedMsg(token: Int): String =
    token2string(token) + " expected but " +token2string(in.token) + " found."

  def scriptUnit(): Tree = {
    in.nextToken()
    if (isLet) {
      in.nextToken()
      val id = in.currToken
      accept(ASSIGNMENT)
      val rhs: Literal = {
        val tokenVal = in.currToken.value
        in.token match {
          case INTLIT => IntLit(tokenVal.toInt)
          case DOUBLELIT => DoubleLit(tokenVal.toDouble)
          case CHARLIT => CharLit(in.ch)
          case STRINGLIT => StringLit(tokenVal)
          case IDENTIFIER => IdentLit(raw(tokenVal))
        }
      }

      Let(raw(id.value), Lit(rhs))
    } else {
      syntaxError(in.offset, "expected let binding, but found " + Tokens.token2string(in.token), false)
      EmptyTree
    }
  }

  final def stats(): Seq[Tree] = {
    ???
  }
}

object Parser {
  private val kwTokenMap: Map[String, Int] = HashMap()
}