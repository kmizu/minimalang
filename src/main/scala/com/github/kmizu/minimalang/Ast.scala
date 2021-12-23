package com.github.kmizu.minimalang

object Ast {
  case class Program(body: Expression)
  sealed abstract class Expression
  sealed case class BinaryExpression(operator: Operator, lhs: Expression, rhs: Expression)
    extends Expression
  case class Sequence(expressions: List[Expression]) extends Expression
  case class If(cond: Expression, thenClause: Expression, elseClause: Expression) extends Expression
  case class While(cond: Expression, body: Expression) extends Expression
  case class Id(name: String) extends Expression
  case class Assignment(name: String, expression: Expression) extends Expression
  case class Literal(value: Int) extends Expression
  case class Println(message: String, value: Expression) extends Expression

  sealed abstract class Operator(name: String)
  object Operator {
    case object Add extends Operator("+")
    case object Subtract extends Operator("-")
    case object Multiply extends Operator("*")
    case object Divide extends Operator("/")
    case object Lt extends Operator("<")
    case object Lte extends Operator("<=")
    case object Gt extends Operator(">")
    case object Gte extends Operator(">=")
    case object Eq extends Operator("==")
    case object NEq extends Operator("!=")
  }
}
