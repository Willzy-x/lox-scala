package com.craftinginterpreters


import java.util
trait LoxCallable {
  def arity(): Int

  def call(interpreter: Interpreter, arguments: util.List[Object]): Object
}
