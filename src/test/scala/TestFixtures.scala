package edu.luc.cs.laufer.cs371.expressions

object TestFixtures {

  import ast.Expr.*

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    )

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    )

  val complex3: Block =
    Block(
      Assignement(
        Identifier("x")
          Constant(5)
      )
    )

  val complex3String = "x = 5;"

  val complex4: Block =
    Block(
      Assignment(
        Identifier("x")
          Constant(5)),
      Assignment(
        Identifier("y")
          Constant(7)
      )
    )

  val complex4String = "x =5 ; y =7;"

}
