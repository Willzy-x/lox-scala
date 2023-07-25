package com.craftinginterpreters

import tool.*

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util
import scala.util.boundary.break

object Lox {
  private var hadError: Boolean = false
  private var hadRuntimeError: Boolean = false
  private val interpreter = Interpreter()

  private def report(line: Int, where: String, message: String): Unit = {
    Console.err.printf(s"[line: $line] Error $where: $message.\n")
  }

  def error(token: Token, message: String): Unit = {
    if (token.tokenType == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, " at '" + token.lexeme + "'", message)
    }
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
    hadError = true
  }
  
  def runtimeError(error: RuntimeError): Unit = {
    Console.err.printf(s"${error.getMessage}\n[line: ${error.token.line}]\n")
    hadRuntimeError = true
  }

  private def run(source: String): Unit = {
    val scanner: Scanner = Scanner(source)
    // Print all tokens
    val tokens: util.List[Token] = scanner.scanTokens()
    val parser: Parser = Parser(tokens)
    val statements: util.List[Stmt] = parser.parse()
    
    interpreter.interpret(statements)
  }

  @throws(classOf[IOException])
  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))

    // Indicate an error in the exit code
    if (this.hadError) System.exit(65)
    if (this.hadRuntimeError) System.exit(70)
  }

  @throws(classOf[IOException])
  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    var line: String = ""
    while (line != null) {
      line = reader.readLine()
      if (line != null) {
        run(line)
      }
      this.hadError = false
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: jlox [script]")
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }
}