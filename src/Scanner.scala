package com.craftinginterpreters

import TokenType.*

import java.{lang, util}
import scala.collection.mutable

object Keywords {
  private val keywords = mutable.HashMap[String, TokenType](
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )

  def get(key: String): TokenType = {
    keywords.get(key) match
      case Some(tokenType) => tokenType
      case None => null
  }
}

class Scanner(val source: String, val tokens: util.List[Token] = new util.ArrayList[Token]() ) {
  private var start = 0
  private var current = 0
  private var line = 1

  private def isDigit(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  private def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
      c == '_'
  }

  private def isAlphaNumeric(c: Char): Boolean = {
    isDigit(c) || isAlpha(c)
  }

  private def isAtEnd: Boolean = {
    current >= source.length
  }

  private def peek(): Char = {
    if (isAtEnd) return 0
    source.charAt(current)
  }

  private def peekNext(): Char = {
    if (current + 1 >= source.length) return 0
    source.charAt(current + 1)
  }

  private def advance(): Char = {
    val ret = source.charAt(current)
    current += 1
    ret
  }

  private def `match`(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source.charAt(current) != expected) return false

    current += 1
    true
  }

  private def string(): Unit = {
    while (peek() != '"' && !isAtEnd) {
      if (peek() == '\n') line += 1
      advance()
    }

    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
      return
    }

    advance() // The closing ".

    // Trim the surrounding quotes.
    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)
  }

  private def number(): Unit = {
    while (isDigit(peek())) advance()

    // Look for a fractional part.
    if (peek() == '.' && isDigit(peekNext())) {
      advance()

      while (isDigit(peek())) advance()
    }
    
    addToken(NUMBER, Double.box(source.substring(start, current).toDouble)) // TODO:  
  }

  private def identifier(): Unit = {
    while (isAlphaNumeric(peek())) advance()

    val text = source.substring(start, current)
    var `type` = Keywords.get(text)
    if (`type` == null) `type` = IDENTIFIER
    addToken(`type`)
  }

  private def addToken(`type`: TokenType): Unit = {
    addToken(`type`, null)
  }

  private def addToken(`type`: TokenType, literal: Object): Unit = {
    val text = source.substring(start, current)
    tokens.add(Token(`type`, text, literal, line))
  }

  private def scanToken(): Unit = {
    val c = advance()
    c match {
      // Single character.
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      // Two character operators.
      case '!' => addToken(if `match`('=') then BANG_EQUAL else BANG)
      case '=' => addToken(if `match`('=') then EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if `match`('=') then LESS_EQUAL else LESS)
      case '>' => addToken(if `match`('=') then GREATER_EQUAL else GREATER)
      case '/' =>
        if (`match`('/')) {
          // A comment goes until the end of the line
          while (peek() != '\n' && !isAtEnd) {
            advance()
          }
        } else {
          addToken(SLASH)
        }
      // Ignore whitespace.
      case ' ' | '\t' | '\r' =>

      case '\n' => line += 1
      case '"' => string()
      case other =>
        if (isDigit(other)) {
          number()
        } else if (isAlpha(other)) {
          identifier()
        } else {
          Lox.error(line, "Unexpected character.")
        }
    }
  }

  def scanTokens(): util.List[Token]  = {
    while (!isAtEnd) {
      start = current
      scanToken()
    }
    tokens.add(Token(EOF, "", null, line))
    tokens
  }
}
