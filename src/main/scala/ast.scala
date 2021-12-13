package edu.luc.cs.laufer.cs371.expressions.ast

/** An initial algebra of arithmetic expressions. */
enum Expr derives CanEqual:
  case Constant(value: Int)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)

  case Identifier(value: String) /** ident::= [a-zA-Z] [a-zA-Z0-9]* */
  case Assignment(left: Expr, right: Expr)
  case Conditional(guard: Expr, left: Expr, right: Expr)
  case Loop(left: Expr, right: Expr)
  case Block(expr: Expr*)

