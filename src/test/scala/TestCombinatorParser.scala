package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import TestFixtures.*
import edu.luc.cs.laufer.cs371.expressions.ast.Expr

object MainCombinatorParser {
  def main(args: Array[String]): Unit = {
    val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
    println(parsedExpr.get)
    println(complex1)
    println(parsedExpr.get == complex1)
    println(behaviors.evaluate(parsedExpr.get))
  }
}

class TestCombinatorParser extends AnyFunSuite {
  val parsedExpr: CombinatorParser.ParseResult[Expr] = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2: CombinatorParser.ParseResult[Expr] = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedExprComp2: CombinatorParser.ParseResult[Expr] = CombinatorParser.parseAll(CombinatorParser.expr, complex2string)
  test("parser works 1") { assert(parsedExpr.get == complex1) }
  test("parser works 2") { assert(parsedExpr2.get == complex1) }
  test("parser works for complex2String") { assert(parsedExprComp2.get == complex2) }

  val parsedExpr3: CombinatorParser.ParseResult[Expr] = CombinatorParser.parseAll(CombinatorParser.top, complex3String)
  val parsedExpr4: CombinatorParser.ParseResult[Expr] = CombinatorParser.parseAll(CombinatorParser.top, complex4String)
  test("parser works for complex3String") { assert(parsedExpr3.get == complex3) }
  test("parser works for complex4String") { assert(parsedExpr4.get == complex4) }



}
