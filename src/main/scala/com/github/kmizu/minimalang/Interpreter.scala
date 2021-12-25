package com.github.kmizu.minimalang

import com.github.kmizu.minimalang.Ast
import com.github.kmizu.minimalang.Ast.Expression
import com.github.kmizu.minimalang.Parser._

import scala.collection.mutable.Map

object Interpreter {
  def interpret(program: Ast.Program): Int = {
    val environment: Map[String, Int] = Map.empty
    def interpretMain(expression: Ast.Expression): Int = expression match {
      case Ast.Id(name) =>
        environment(name)
      case Ast.Assignment(name, expression) =>
        val value = interpretMain(expression)
        environment.put(name, value)
        value
      case Ast.Literal(value) =>
        value
      case Ast.Println(message, value) =>
        val result = interpretMain(value)
        println(s"${message}:${result}")
        result
      case Ast.If(cond, thenClause, elseClause) =>
        val condResult = interpretMain(cond)
        if(condResult != 0) {
          interpretMain(thenClause)
        } else {
          interpretMain(elseClause)
        }
      case Ast.Sequence(expressions) =>
        val head = expressions.head
        val tail = expressions.tail
        tail.foldLeft(interpretMain(head)){(a, b) => interpretMain(b)}
      case Ast.While(cond, body) =>
        def go(): Int = if(interpretMain(cond) != 0) {
          interpretMain(body)
          go()
        } else {
          0
        }
        go()
      case Ast.BinaryExpression(operator, lhs, rhs) =>
        val lhsResult = interpretMain(lhs)
        val rhsResult = interpretMain(rhs)
        import Ast.Operator._
        operator match {
          case Add => lhsResult + rhsResult
          case Subtract => lhsResult - rhsResult
          case Multiply => lhsResult * rhsResult
          case Divide => lhsResult / rhsResult
          case Lt => if(lhsResult < rhsResult) 1 else 0
          case Lte => if(lhsResult <= rhsResult) 1 else 0
          case Gt => if(lhsResult > rhsResult) 1 else 0
          case Gte => if(lhsResult >= rhsResult) 1 else 0
          case Eq => if(lhsResult == rhsResult) 1 else 0
          case NEq => if(lhsResult != rhsResult) 1 else 0
        }
    }
    interpretMain(program.body)
  }
  def interpretAll(programText: String): Int = {
    val program = parseAll(programText)
    interpret(program)
  }
}
