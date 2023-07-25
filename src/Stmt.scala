package com.craftinginterpreters

import java.util

abstract class Stmt {
  def accept[A](visitor: Stmt.Visitor[A]): A
}

class Expression(val expression: Expr) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitExpressionStmt(this)
  }
}

class Print(val expression: Expr) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitPrintStmt(this)
  }
}

class Var(val name: Token, val initializer: Expr) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitVarStmt(this)
  }
}

class Block(val statements: util.List[Stmt]) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitBlockStmt(this)
  }
}

object Stmt {
 trait Visitor[A]:
   def visitExpressionStmt(stmt: Expression): A
   
   def visitPrintStmt(stmt: Print): A
   
   def visitVarStmt(stmt: Var): A
   
   def visitBlockStmt(stmt: Block): A
}