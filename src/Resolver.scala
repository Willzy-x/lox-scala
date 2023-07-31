package com.craftinginterpreters

import java.util
import scala.util.boundary
import scala.util.boundary.break
import scala.collection.mutable

class Resolver(private val interpreter: Interpreter) extends Expr.Visitor[Unit], Stmt.Visitor[Unit] {
  private val scopes: mutable.Stack[mutable.HashMap[String, Boolean]] = mutable.Stack()
  private var currentFunction: FunctionType = FunctionType.NONE

  private def resolve(stmt: Stmt): Unit = stmt.accept(this)

  private def resolve(expr: Expr): Unit = expr.accept(this)

  private def beginScope(): Unit = {
    scopes.push(mutable.HashMap[String, Boolean]())
  }

  private def endScope(): Unit = {
    scopes.pop()
  }

  private def declare(name: Token): Unit = {
    if (scopes.isEmpty) return

    val scope = scopes.top
    if (scope.contains(name.lexeme)) {
      Lox.error(name, "Already a variable with this name in this scope.")
    }
    scope.put(name.lexeme, Boolean.box(false))
  }

  private def define(name: Token): Unit = {
    if (scopes.isEmpty) return

    val scope = scopes.top
    scope.put(name.lexeme, Boolean.box(true))
  }

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    // Start at the innermost scope and work outwards
    var depth: Int = 0

    boundary:
      scopes.foreach(scope =>
        if (scope.contains(name.lexeme)) {
          interpreter.resolve(expr, depth)
          break()
        }
        depth += 1
      )
  }

  private def resolveFunction(function: Func, `type`: FunctionType): Unit = {
    val enclosingFunction = currentFunction
    currentFunction = `type`

    beginScope()
    function.params.forEach(param => {
      declare(param)
      define(param)
    })
    resolve(function.body)
    endScope()
    currentFunction = enclosingFunction
  }

  def resolve(statements: util.List[Stmt]): Unit = {
    statements.forEach(resolve)
  }

  override def visitBinaryExpr(expr: Binary): Unit = {
    resolve(expr.left)
    resolve(expr.right)
  }

  override def visitCallExpr(expr: Call): Unit = {
    resolve(expr.callee)
    expr.arguments.forEach(arg => resolve(arg))
  }

  override def visitGroupingExpr(expr: Grouping): Unit = {
    resolve(expr.expression)
  }

  override def visitUnaryExpr(expr: Unary): Unit = {
    resolve(expr.right)
  }

  override def visitLogicalExpr(expr: Logical): Unit = {
    resolve(expr.left)
    resolve(expr.right)
  }

  override def visitLiteralExpr(expr: Literal): Unit = {}

  override def visitVariableExpr(expr: Variable): Unit = {
    var pass = false
    if (scopes.nonEmpty) {
      val res = scopes.top.get(expr.name.lexeme)
      res match
        case Some(i) => if (i.eq(false)) pass = false else pass = true
        case None => pass = true
    } else {
      pass = true
    }

    if (!pass) Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name)
  }

  override def visitAssignExpr(expr: Assign): Unit = {
    resolve(expr.value)
    resolveLocal(expr, expr.name)
  }

  override def visitExpressionStmt(stmt: Expression): Unit = {
    resolve(stmt.expression)
  }

  override def visitFunctionStmt(stmt: Func): Unit = {
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(stmt, FunctionType.FUNCTION)
  }

  override def visitPrintStmt(stmt: Print): Unit = {
    resolve(stmt.expression)
  }

  override def visitReturnStmt(stmt: Return): Unit = {
    if (currentFunction == FunctionType.NONE) {
      Lox.error(stmt.keyword, "Can't return from top-level code.")
    }

    if (stmt.value != null)
      resolve(stmt.value)
  }

  override def visitVarStmt(stmt: Var): Unit = {
    declare(stmt.name)
    if (stmt.initializer != null) {
      resolve(stmt.initializer)
    }

    define(stmt.name)
  }

  override def visitBlockStmt(stmt: Block): Unit = {
    beginScope()
    resolve(stmt.statements)
    endScope()
  }

  override def visitIfStmt(stmt: If): Unit = {
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if (stmt.elseBranch != null) resolve(stmt.elseBranch)
  }

  override def visitWhileStmt(stmt: While): Unit = {
    resolve(stmt.condition)
    resolve(stmt.body)
  }
}

private enum FunctionType:
  case
    NONE, FUNCTION