package com.craftinginterpreters

import TokenType.*

import java.util
import java.lang
import scala.util.boundary
import scala.util.boundary.break
import Parser.ParserError

object Parser {
  class ParserError extends RuntimeException {}
}

class Parser(val tokens: util.List[Token]) {
  private var current: Int = 0

  private def peek(): Token =
    tokens.get(current)

  private def isAtEnd: Boolean =
    peek().tokenType == EOF

  private def check(`type`: TokenType): Boolean = {
    if (isAtEnd) return false
    peek().tokenType == `type`
  }

  private def advance(): Token = {
    val ret = tokens.get(current)
    current += 1
    ret
  }

  private def previous(): Token =
    tokens.get(current - 1)

  private def error(token: Token, message: String): ParserError = {
    Lox.error(token, message)
    ParserError()
  }

  private def synchronize(): Unit = {
    advance()
    boundary:
      while (!isAtEnd) {
        if (previous().tokenType == SEMICOLON) break()
        peek().tokenType match  {
          case CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE => break()
          case other => advance()
        }
      }
  }

  private def `match`(types: TokenType *): Boolean = {
    // Non-local returns are not supported
    boundary:
      for (`type` <- types) {
        if (check(`type`)) {
          advance()
          break(true)
        }
      }
      false
  }

  private def consume(`type`: TokenType, message: String): Token = {
    if (check(`type`)) return advance()

    throw error(peek(), message)
  }

  private def declaration(): Stmt = {
    try {
      if (`match`(VAR)) return varDeclaration()
      statement()
    } catch {
      case e: ParserError => synchronize()
      null
    }
  }
  
  private def statement(): Stmt = {
    if (`match`(IF)) return ifStatement()
    if (`match`(PRINT)) return printStatement()
    if (`match`(LEFT_BRACE)) return Block(block())
    
    expressionStatement()
  }

  private def block(): util.List[Stmt] = {
    val statements: util.List[Stmt] = util.ArrayList()

    while (!check(RIGHT_BRACE) && !isAtEnd) {
      statements.add(declaration())
    }

    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements
  }

  private def ifStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch = statement()
    var elseBranch: Stmt = null
    if (`match`(ELSE)) {
      elseBranch = statement()
    }

    If(condition, thenBranch, elseBranch)
  }
  
  private def printStatement(): Stmt = {
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(value)
  }

  private def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect variable name.")

    var initializer: Expr = null
    if (`match`(EQUAL)) {
      initializer = expression()
    }

    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, initializer)
  }
  
  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  private def expression(): Expr = {
    assignment()
  }

  private def assignment(): Expr = {
    val expr = equality()

    if (`match`(EQUAL)) {
      val equals = previous()
      val value = assignment()

      expr match
        case e: Variable =>
          val name = e.name
          return Assign(name, value)
        case other => error(equals, "Invalid assignment target.")
    }
    expr
  }

  private def equality(): Expr = {
    var expr = comparison()
    while (`match`(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def comparison(): Expr = {
    var expr = term()
    while (`match`(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def term(): Expr = {
    var expr = factor()
    while (`match`(MINUS, PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def factor(): Expr = {
    var expr = unary()
    while (`match`(SLASH, STAR)) {
      val operator = previous()
      val right = unary()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def unary(): Expr = {
    if (`match`(BANG, MINUS)) {
      val operator = previous()
      val right = unary()
      return Unary(operator, right)
    }
    primary()
  }

  private def primary(): Expr = {
    if (`match`(FALSE)) return Literal(Boolean.box(false))
    if (`match`(TRUE)) return Literal(Boolean.box(true))
    if (`match`(NIL)) return Literal(null)

    if (`match`(NUMBER, STRING)) {
      return Literal(previous().literal)
    }

    if (`match`(IDENTIFIER)) {
      return Variable(previous())
    }

    if (`match`(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return Grouping(expr)
    }

    throw error(peek(), "Expect expression")
  }

  def parse(): util.List[Stmt] = {
    val statements: util.List[Stmt] = util.ArrayList()
    while (!isAtEnd) {
      statements.add(declaration())
    }
    
    statements
  }
}
