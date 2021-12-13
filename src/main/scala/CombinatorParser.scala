package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast.Expr
import Expr.*

object CombinatorParser extends JavaTokenParsers {

  /**
   * Enable missing typesafe equality between `None` and `Option`.
   * TODO remove once the library provides this.
   */
  given CanEqual[None.type, Option[_]] = CanEqual.derived

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => UMinus(e) }
      | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    )

  /** statement ::= expressions ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ {case e ~ _ => e}
      | assignment
      | conditional
      | loop
      | block
    )


  /** assignment ::= indent "=" expression ";" */
  def assignment: Parser[Expr] =(
    ident ~ "=" ~ expr <~ ";" ^^ {case l ~ _ ~ r => Assignment(Identifier(l), r) }
    )

  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Expr] =
    ("if" ~ "(" ~> expr ~ ")" ~ block) ~! opt(("else") ~ block) ^^ {
      case a ~ _ ~ b ~ None => Conditional(a,b, Block())
      case a ~ _ ~ b ~ Some("else" ~ r) => Conditional(a, b, r)
    }

  /** loop ::= "while" "(" expressions ")" block */
  def loop: Parser[Expr] = {
    "while" ~ "(" ~> expr ~ ")" ~ block ^^ {case e ~ _ ~ b => Loop(e, b)}
  }

  /** block ::= "{" statement "}" */
  def block: Parser[Expr] ={
    "{" ~> rep(statement) <~ "}" ^^ {case e => Block(e: _*)}
  }

  def top: Parser[Expr] = {
    rep(statement) ^^ {case e => Block(e: _*)}
  }
}
