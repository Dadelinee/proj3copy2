package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite

import behaviors.*
import TestFixtures.*

object Main {
  def main(args: Array[String]): Unit = {
    println("p = " + complex1)
    println("evaluate(p) = " + evaluate(store)(complex1))
    println("size(p) = " + size(complex1))
    println("height(p) = " + height(complex1))
    println(toFormattedString(complex1))
    println(toUnParser(complex1))
    println(store)

    println("q = " + complex2)
    println("evaluate(q) = " + evaluate(store)(complex2))
    println("size(q) = " + size(complex2))
    println("height(q) = " + height(complex2))
    println(toFormattedString(complex2))
    println(toFormattedString(complex2))
    println(toUnParser(complex2))
    println(store)

    println("a = " + complex3)
    println("evaluate(a) = " + evaluate(store)(complex3))
    println("size(a) = " + size(complex3))
    println("height(a) = " + height(complex3))
    println(toFormattedString(complex3))
    println(toFormattedString(complex3))
    println(toUnParser(complex3))
    println(store)

    println("b = " + complex4)
    println("evaluate(b) = " + evaluate(store)(complex4))
    println("size(b) = " + size(complex4))
    println("height(b) = " + height(complex4))
    println(toFormattedString(complex4))
    println(toFormattedString(complex4))
    println(toUnParser(complex4))
    println(store)
  }
}

class Test extends AnyFunSuite {
  test("evaluate(p)") { assert(evaluate(store)(complex1) === Try(-1)) }
  test("size(p)") { assert(size(complex1) == 9) }
  test("height(p)") { assert(height(complex1) == 4) }


  test("evaluate(q)") { assert(evaluate(store)(complex2) === Try(0)) }
  test("size(q)") { assert(size(complex2) == 10) }
  test("height(q)") { assert(height(complex2) == 5) }


  test("evaluate(a)") { assert(evaluate(store)(complex3) === Try(0)) }
  test("size(a)") { assert(size(complex3) == 3) }
  test("height(a)") { assert(height(complex3) == 3) }


  test("evaluate(b)") { assert(evaluate(store)(complex4) === Try(0)) }
  test("size(b)") { assert(size(complex4) == 6) }
  test("height(b)") { assert(height(complex4) == 3) }
}

