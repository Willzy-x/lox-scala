package com.craftinginterpreters

class RuntimeError(val token: Token, message: String) extends RuntimeException(message) {
}
