package edu.luc.cs.laufer.cs371.expressions

import ast.Expr
import Expr.*

import scala.util.{Success, Try, Failure}

object behaviors {
  type Store = Map[String, Int]
  type Result = Try[Int]

  def evaluate(store: Store)(e: Expr): Result = e match{
    case Constant(value)       => Success(value)
    case UMinus(um)            =>
      for um <- evaluate(store)(um) yield -um

    case Plus(l, r)            =>
      for
        l <- evaluate(store)(l)
        r <- evaluate(store)(r)
      yield l + r

    case Minus(l, r)           =>
      for
        l <- evaluate(store)(l)
        r <- evaluate(store)(r)
      yield l - r

    case Times(l, r)           =>
      for
      l <- evaluate(store)(l)
      r <- evaluate(store)(r)
     yield l * r

    case Div(l, r)             =>
      for
        l <- evaluate(store)(l)
        r <- evaluate(store)(r)
      yield l / r

    case Mod(l, r)             =>
      for
        l <- evaluate(store)(l)
        r <- evaluate(store)(r)
      yield l % r

    case Indentifier(name)     => Try(store(name))

    case Assignment(l, r)      =>
      for
        rvalue <- evaluate(store)(r)
        Identifier(name) = l
        result = store.update(name, rvalue)
      yield 0

    case Conditional(guard, l, r)    => evaluate(store)(guard) match {
      case Success(0)       => evaluate(store)(r)
      case Success(_)       => evaluate(store)(l)
      case f @ Failure(_)   => f
    }

    case Loop(guard, body)           =>
      while true do {
      evaluate(store)(guard) match {
        case Success(0)      => return Success(0)
        case Success(v)      =>
          evaluate(store)(body) match {
            case f2 @ Failure(fb)   => return f2
            case _                  =>
          }
        case f @ Failure(_)         => return f
      }
    }
      Success(0)

    case Block(elements@_*)      =>
      val i = elements.iterator
      var result: Result = Success(0)
      while i.hasNext do {
      evaluate(store)(i.next()) match {
        case s @ Success(r)     => result = s
        case f @ Failure(_)     => return f
      }
    }
      result
  }

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)

    case Identifier(c)         => 1
    case Assignment(l, r)      => 1 + size(l) + size(r)
    case Conditional(g, l, r)  => 1 + size(g) + size(l) + size(r)
    case Loop(l, r)            => 1 + size(l) + size(r)
    case Block(elements@_*)    => elements.map(c => size(c)).sum

  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))

    case Identifier(c)         => 1
    case Assignment(l, r)      => 1 + math.max(height(l), height(r))
    case Conditional(g, l, r)  => 1 + math.max(height(g), height(l), height(r))
    case Loop(l, r)            => 1 + math.max(height(l), height(r))
    case Block(elements@_*)    => elements.map(c => math.max(height(c)))
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c) => "Constant(" + c.toString + ")"
    case UMinus(r)   => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)  => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)   => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)   => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

    case Indentifier(c)        => prefix + c
    case Assignment(l, r)      => buildExprString(prefix, "Assignment", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Conditional(g, l ,r) => buildConditionalString(prefix, "Conditional", toFormattedString(prefix + INDENT)(g), toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Loop(l, r)            => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Block(elements@_*)    => "Block(" + elements.map(c => toFormattedString(c).mkString + ")")
  }

  def toUnParser(prefix: String)(e: Expr): String = e match {
    case Constant(c)               => c.toString
    case UMinus(r)                 => "-" + toUnParser(r)
    case Plus(l, r)                => toUnParser(l) + " + " + toUnParser(r)
    case Minus(l, r)               => toUnParser(l) + " - " + toUnParser(r)
    case Times(l, r)               => toUnParser(l) + " * " + toUnParser(r)
    case Div(l, r)                 => toUnParser(l) + " / " + toUnParser(r)
    case Mod(l, r)                 => toUnParser(l) + " % " + toUnParser(r)
    case Indentifier(c)            => c
    case Assignment(l, r)          => toUnParser(l) + " = " + toUnParser(r) + ";\n"
    case Conditional(g, l, r)      => "if (" + toUnParser(g) + ") {\n" + toUnParser(l) + "\n} else {\n" + toUnParser(r) + "\n}"
    case Loop(l, r)                => "while (" + toUnParser(l) + ") {\n" + toUnParser(r) + "}"
    case Block(elements@_*)        => elements.map(c => toUnParser(c)).mkString
  }

  def toUnParser(e: Expr): String = toUnParser("")(e)

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildConditionalString(prefix: String, nodeString: String, guardString: String, leftString: String, rightString: String): String = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(guardString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
