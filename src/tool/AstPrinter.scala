package com.craftinginterpreters
package tool

class AstPrinter extends Expr.Visitor[String] {
  private def parenthesize(name: String, exprs: Expr *): String = {
    val builder = StringBuilder()

    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ").append(expr.accept(this))
    }
    builder.append(")")
    builder.toString()
  }

  def print(expr: Expr): String = {
    expr.accept[String](this)
  }

  override def visitBinaryExpr(expr: Binary): String = {
    parenthesize(expr.operator.lexeme, expr.left, expr.right)
  }

  override def visitGroupingExpr(expr: Grouping): String = {
    parenthesize("group", expr.expression)
  }

  override def visitUnaryExpr(expr: Unary): String = {
    parenthesize(expr.operator.lexeme, expr.right)
  }

  override def visitLiteralExpr(expr: Literal): String = {
    if (expr.value == null) return "nil"
    expr.value.toString
  }

  override def visitVariableExpr(expr: Variable): String = {
    parenthesize(expr.name.lexeme, null)
  }

  override def visitAssignExpr(expr: Assign): String = {
    parenthesize(expr.name.lexeme, expr.value)
  }

  override def visitLogicalExpr(expr: Logical): String = {
    parenthesize(expr.operator.lexeme, expr.left, expr.right)
  }
}

@main def printTest(): Unit = {
  val expression = Binary(
    Unary(
      Token(TokenType.MINUS, "-", null, 1),
      Literal(java.lang.Integer(123))),
      Token(TokenType.STAR, "*", null, 1),
    Grouping(Literal(java.lang.Double(45.67)))
  )

  println(AstPrinter().print(expression))
}
