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

class Func(val name: Token, val params: util.List[Token], val body: util.List[Stmt]) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitFunctionStmt(this)
  }
}

class If(val condition: Expr, val thenBranch: Stmt, val elseBranch: Stmt) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitIfStmt(this)
  }
}

class Print(val expression: Expr) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitPrintStmt(this)
  }
}

class Return(val keyword: Token, val value: Expr) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitReturnStmt(this)
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

class While(val condition: Expr, val body: Stmt) extends Stmt {
  override def accept[A](visitor: Stmt.Visitor[A]): A = {
    visitor.visitWhileStmt(this)
  }
}

object Stmt {
 trait Visitor[A]:
   def visitExpressionStmt(stmt: Expression): A
   
   def visitFunctionStmt(stmt: Func): A
   
   def visitPrintStmt(stmt: Print): A
   
   def visitReturnStmt(stmt: Return): A
   
   def visitVarStmt(stmt: Var): A
   
   def visitBlockStmt(stmt: Block): A
   
   def visitIfStmt(stmt: If): A
   
   def visitWhileStmt(stmt: While): A
}