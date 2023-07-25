package com.craftinginterpreters

import Stmt.Visitor

abstract class Expr {
  def accept[A](visitor: Expr.Visitor[A]): A
}

class Binary(val left: Expr, val operator: Token, val right: Expr) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitBinaryExpr(this)
  }
}

class Grouping(val expression: Expr) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitGroupingExpr(this)
  }
}

class Literal(val value: Object) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitLiteralExpr(this)
  }
}

class Unary(val operator: Token, val right: Expr) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitUnaryExpr(this)
  }
}

class Variable(val name: Token) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitVariableExpr(this)
  }
}

class Assign(val name: Token, val value: Expr) extends Expr {
  override def accept[A](visitor: Expr.Visitor[A]): A = {
    visitor.visitAssignExpr(this)
  }
}

object Expr {
  trait Visitor[A]:
    def visitBinaryExpr(expr: Binary): A

    def visitGroupingExpr(expr: Grouping): A

    def visitUnaryExpr(expr: Unary): A

    def visitLiteralExpr(expr: Literal): A
    
    def visitVariableExpr(expr: Variable): A

    def visitAssignExpr(expr: Assign): A
}