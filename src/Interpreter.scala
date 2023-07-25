package com.craftinginterpreters

import TokenType.*

import java.{lang, util}

class Interpreter extends Expr.Visitor[Object], Stmt.Visitor[Unit] {
  private var environment = Environment()
  
  private def evaluate(expr: Expr): Object = {
    expr.accept(this)
  }
  
  private def execute(stmt: Stmt): Unit = {
    stmt.accept(this)
  }

  private def executeBlock(statements: util.List[Stmt], environment: Environment): Unit = {
    val previous = this.environment
    try {
      this.environment = environment

      statements.forEach(statement => execute(statement))
    } finally {
      this.environment = previous
    }
  }

  private def isTruthy(`object`: Object): Boolean = {
    if (`object` == null) return false
    `object` match
      case o: lang.Boolean => o
      case _ => true
  }

  private def isEqual(a: Object, b: Object): Boolean = a == b
  
  private def stringify(`object`: Object): String = {
    if (`object` == null) return "nil"
    
    `object` match
      case o: lang.Double =>
        var text = o.toString
        if (text.endsWith(".0")) {
          text = text.substring(0, text.length - 2)
        }
        text
      case other => `object`.toString
  }

  private def checkNumberOperand(operator: Token, operand: Object): Unit = {
    operand match
      case o: lang.Double => // Do nothing
      case other => throw RuntimeError(operator, "Operand must be a number.")
  }

  private def checkNumberOperands(operator: Token, left: Object, right: Object): Unit = {
    (left, right) match
      case (l: lang.Double, r: lang.Double) => // Do nothing
      case other => throw RuntimeError(operator, "Operands must be numbers.")
  }

  override def visitBinaryExpr(expr: Binary): Object = {
    // evaluate in left to right order
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match {
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) > Double.unbox(right)
        return Boolean.box(res)
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) >= Double.unbox(right)
        return Boolean.box(res)
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) < Double.unbox(right)
        return Boolean.box(res)
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) <= Double.unbox(right)
        return Boolean.box(res)
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) - Double.unbox(right)
        return Double.box(res)
      case BANG_EQUAL => return Boolean.box(!isEqual(left, right))
      case EQUAL_EQUAL => return Boolean.box(isEqual(left, right))
      case PLUS =>
        (left, right) match
          case (l: lang.Double, r: lang.Double) =>
            val res = Double.unbox(l) + Double.unbox(r)
            return Double.box(res)
          case (l: String, r: String) =>
            return String(l) + String(r)
          case (_, _) => throw RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) / Double.unbox(right)
        return Double.box(res)
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        val res = Double.unbox(left) * Double.unbox(right)
        return Double.box(res)
      case other => // Not reachable
    }

    null
  }

  override def visitGroupingExpr(expr: Grouping): Object = evaluate(expr.expression)

  override def visitUnaryExpr(expr: Unary): Object = {
    val right = evaluate(expr.right)

    expr.operator.tokenType match
      case BANG =>
        return Boolean.box(!isTruthy(right))
      case MINUS =>
        val neg = Double.unbox(right)
        return Double.box(-neg)
      case other => // Not reachable

    null
  }

  override def visitLogicalExpr(expr: Logical): Object = {
    val left = evaluate(expr.left)

    if (expr.operator.tokenType == OR) {
      if (isTruthy(expr.left)) return left
    } else {
      if (!isTruthy(expr.left)) return left
    }

    evaluate(expr.right)
  }

  override def visitLiteralExpr(expr: Literal): Object = expr.value

  override def visitVariableExpr(expr: Variable): Object = {
    environment.get(expr.name)
  }

  override def visitAssignExpr(expr: Assign): Object = {
    val value = evaluate(expr.value)
    environment.assign(expr.name, value)
    value
  }

  override def visitIfStmt(stmt: If): Unit = {
    if (isTruthy(stmt.condition)) {
      execute(stmt.thenBranch)
    } else if (stmt.elseBranch != null) {
      execute(stmt.elseBranch)
    }
  }

  override def visitExpressionStmt(stmt: Expression): Unit = {
    evaluate(stmt.expression)
  }

  override def visitPrintStmt(stmt: Print): Unit = {
    val value = evaluate(stmt.expression)
    println(stringify(value))
  }

  override def visitWhileStmt(stmt: While): Unit = {
    while (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.body)
    }
  }

  override def visitVarStmt(stmt: Var): Unit = {
    var value: Object = null
    if (stmt.initializer != null) {
      value = evaluate(stmt.initializer)
    }

    environment.define(stmt.name.lexeme, value)
  }

  override def visitBlockStmt(stmt: Block): Unit = {
    executeBlock(stmt.statements, Environment(environment))
  }

  def interpret(statements: util.List[Stmt]): Unit = { 
    try {
      statements.forEach(i => if (i != null) execute(i))
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }
  }

}

@main def testBoolean(): Unit = {
  // Test Boolean object type matching
  val a: Object = Boolean.box(true)
  a match
    case o: lang.Boolean => println(s"Matched ${o.getClass.getSimpleName}")
    case other => println("Matched other types")
  // Test Double object type matching
  var b: Object = Double.box(1.1)
  var neg = Double.unbox(b)
  neg = -neg
  b = Double.box(neg)
  b match
    case o: lang.Double => println(s"Matched ${o.getClass.getSimpleName}")
    case other => println("Matched other types")
  // Test Object equality
  val num1 = Double.box(1.12)
  val num2 = Double.box(1.12)
  println(s"num1 equals num2: ${num1.equals(num2)}")
  println(s"num1 eq num2: ${num1.eq(num2)}")
  println(s"num1 == num2: ${num1 == num2}")
  println(s"null == num2: ${null == num2}")
  println(s"null == null: ${null == null}")
}