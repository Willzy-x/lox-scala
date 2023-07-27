package com.craftinginterpreters
import java.util

class LoxFunction(private val declaration: Func, private val closure: Environment) extends LoxCallable {
  override def arity(): Int = declaration.params.size()

  override def toString: String = s"<fn ${declaration.name.lexeme}>"

  override def call(interpreter: Interpreter, arguments: util.List[Object]): Object = {
    val environment = Environment(closure)
    for (i <- (0 until arguments.size())) {
      environment.define(declaration.params.get(i).lexeme, arguments.get(i))
    }

    try {
      interpreter.executeBlock(declaration.body, environment)
    } catch {
      case e: Ret => return e.value
    }
    null
  }
}


@main def test(): Unit = {
  for (i <- 0 until 1) {
    println(i)
  }
}