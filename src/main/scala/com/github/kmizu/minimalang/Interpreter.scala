package com.github.kmizu.minimalang

import com.github.kmizu.minimalang.Ast
import com.github.kmizu.minimalang.Ast.Expression

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
      case Ast.If(cond, thenClause, elseClause) =>
        val condResult = interpretMain(cond)
        if(condResult != 0) {
          interpretMain(thenClause)
        } else {
          interpretMain(elseClause)
        }
      case Ast.Sequence(expressions) =>
        val firstResult = interpretMain(expressions.head)
        val rest = expressions.tail
        rest.foldLeft(firstResult){(a, b) => interpretMain(b)}
      case Ast.While(cond, body) =>
        def go(): Int = if(interpretMain(cond) != 0) {
          interpretMain(body)
          go()
        } else {
          0
        }
        go()
      case Ast.Literal(n) =>
        n
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
      case Ast.Println(message, value) =>
        val result = interpretMain(value)
        println(s"${message}:${result}")
        result
    }
    interpretMain(program.body)
  }
}
