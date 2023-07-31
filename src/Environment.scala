package com.craftinginterpreters

import scala.collection.mutable

class Environment(private val enclosing: Environment = null) {
  private val values = mutable.HashMap[String, Object]()

  def get(name: Token): Object = {
    val ret = values.get(name.lexeme)

    ret match
      case Some(i) => i
      case None =>
        if (enclosing != null) {
          return enclosing.get(name)
        }
        throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }

  def define(name: String, value: Object): Unit = {
    values.addOne((name, value))
  }

  def ancestor(distance: Int): Environment = {
    var environment = this
    for (i <- 0 until distance) {
      environment = environment.enclosing
    }
    environment
  }

  def getAt(distance: Int, name: String): Object = {
    ancestor(distance).values.get(name) match
      case Some(o) => o
      case None => null
  }

  def assignAt(distance: Int, name: Token, value: Object): Unit ={
    ancestor(distance).values.put(name.lexeme, value)
  }

  def assign(name: Token, value: Object): Unit = {
    if (values.contains(name.lexeme)) {
      values.addOne((name.lexeme, value))
      return
    }
    
    if (enclosing != null) {
      enclosing.assign(name, value)
      return 
    }

    throw RuntimeError(name, s"Undefined Variable '${name.lexeme}' .")
  }
}
