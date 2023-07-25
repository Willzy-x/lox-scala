package com.craftinginterpreters


case class Token(val tokenType: TokenType, val lexeme: String, val literal: Object, val line: Int) {

  override def toString: String = {
    tokenType.toString + " " + lexeme + " " + literal
  }
}
