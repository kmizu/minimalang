package com.github.kmizu.minimalang

import com.github.kmizu.minimalang.Ast.Program
import com.github.kmizu.minimalang.Ast._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextualProgramInterpreterTest extends AnyFlatSpec with Matchers {
  "A textual simple program" should "be interpreted correctly" in {
    val text = """3;"""
    val program = Parser.parseAll(text)
    val result = Interpreter.interpret(program)
    result should be (3)
  }
  "A mathematical expression" should "be interpreted correctly" in {
    val text = """(1 - 4) * (3 + 4) / 3;"""
    val program = Parser.parseAll(text)
    val result = Interpreter.interpret(program)
    result should be (-7)
  }
  "An if expression (case true)" should "be interpreted correctly" in {
    val text = """if (1) 3 else 5;"""
    val program = Parser.parseAll(text)
    val result = Interpreter.interpret(program)
    result should be (3)
  }
  "An if expression (case false)" should "be interpreted correctly" in {
    val text = """if (0) 3 else 5;"""
    val program = Parser.parseAll(text)
    val result = Interpreter.interpret(program)
    result should be (5)
  }
  "A while expression" should "be interpreted correctly" in {
    val text =
      """
        |i <- 0;
        |while (i < 10) {
        |  i <- i + 1;
        |};
        |i;""".stripMargin
    val program = Parser.parseAll(text)
    val result = Interpreter.interpret(program)
    result should be (10)
  }
}
