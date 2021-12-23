package com.github.kmizu.minimalang

import com.github.kmizu.minimalang.Ast.Program
import com.github.kmizu.minimalang.Ast._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterTest extends AnyFlatSpec with Matchers {
  "A program that includes sequences" should "be interpreted correctly" in {
    val program = Program(
      Sequence(List(Literal(1)))
    )
    val result = Interpreter.interpret(program)
    result should be (1)
  }
  "A program that includes branches" should "be interpreted correctly" in {
    val program = Program(
      Sequence(List(
        Assignment("x", Ast.Literal(1)),
        If(
          Id("x"),
          Literal(10),
          Literal(20)
        )
      ))
    )
    val result = Interpreter.interpret(program)
    result should be (10)
  }
  "A program that includes loops" should "be interpreted correctly" in {
    val program = Program(
      Sequence(List(
        Assignment("x", Ast.Literal(0)),
        While(
          BinaryExpression(Operator.Lt, Id("x"), Literal(10)),
          Assignment("x", BinaryExpression(Operator.Add, Id("x"), Literal(1)))
        ),
        Id("x")
      ))
    )
    val result = Interpreter.interpret(program)
    result should be (10)
  }
  "A program that includes println" should "be interpreted correctly" in {
    val program = Program(
      Sequence(List(
        Assignment("x", Literal(0)),
        While(
          BinaryExpression(Operator.Lt, Id("x"), Literal(10)),
          Sequence(List(
            Assignment("x", BinaryExpression(Operator.Add, Id("x"), Literal(1))),
            Println("x is", Id("x"))
          )),
        ),
        Id("x")
      ))
    )
    val result = Interpreter.interpret(program)
    result should be (10)
  }
}
