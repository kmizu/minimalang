package com.github.kmizu.minimalang

import com.github.kmizu.minimalang.Ast._

import scala.util.matching.Regex
import com.github.kmizu.scomb._

import scala.collection.mutable

object Parser extends SCombinator {
  implicit def stringToParser(literal: String): Parser[String] = $(literal)

  implicit def regexToParser(literal: Regex): Parser[String] = regularExpression(literal)

  lazy val LINEFEED: Parser[String] = ("\r\n" | "\r" | "\n")

  lazy val SEMICOLON: Parser[String] = ";" << SPACING

  lazy val ANY: Parser[String] = any ^^ {
    _.toString
  }

  lazy val BLOCK_COMMENT: Parser[Any] = rule {
    "/*" ~ (not("*/") ~ (BLOCK_COMMENT | ANY)).* ~ "*/"
  } << SPACING

  lazy val LINE_COMMENT: Parser[Any] = rule {
    "//" ~ (not(LINEFEED) ~ ANY).* ~ LINEFEED
  } << SPACING

  lazy val COMMENT: Parser[Any] = rule {
    BLOCK_COMMENT | LINE_COMMENT
  }

  lazy val SPACING: Parser[String] = rule {
    (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {
      _.mkString
    }
  }

  private[this] val tokenNames = mutable.Set.empty[String]

  def kwToken(name: String): Parser[String] = {
    tokenNames += name
    name << SPACING
  }

  //begin token definition
  val GT: Parser[String] = kwToken(">")
  val LT: Parser[String] = kwToken("<")
  val LARROW: Parser[String] = kwToken("<-")
  val LTE: Parser[String] = kwToken("<=")
  val GTE: Parser[String] = kwToken(">=")
  val PLUS: Parser[String] = kwToken("+")
  val MINUS: Parser[String] = kwToken("-")
  val ASTER: Parser[String] = kwToken("*")
  val SLASH: Parser[String] = kwToken("/")
  val LPAREN: Parser[String] = kwToken("(")
  val RPAREN: Parser[String] = kwToken(")")
  val LBRACE: Parser[String] = kwToken("{")
  val RBRACE: Parser[String] = kwToken("}")
  val IF: Parser[String] = kwToken("if")
  val ELSE: Parser[String] = kwToken("else")
  val WHILE: Parser[String] = kwToken("while")
  val COMMA: Parser[String] = kwToken(",")
  val DOT: Parser[String] = kwToken(".")
  val EQ: Parser[String] = kwToken("=")
  val EQEQ: Parser[String] = kwToken("==")
  val COLON: Parser[String] = kwToken(":")
  val ID: Parser[String]= ("""[a-zA-Z][a-zA-Z0-9_]*""".r).filter { id =>
    !tokenNames.contains(id)
  } << SPACING
  //end token definition

  lazy val KEYWORDS: Set[String] = tokenNames.toSet

  lazy val program: Parser[Program] = rule {
    SPACING >> line.+ ^^ {lines => Program(Sequence(lines))}
  }

  lazy val line: Parser[Expression] = rule {
    (assignment | expression) << SEMICOLON
  }

  lazy val assignment: Parser[Expression] = rule {
    ID ~ LARROW ~ expression ^^ { case name ~ _ ~ init => Assignment(name, init)}
  }

  //expression ::= blockExpression | expression | ifExpression | whileExpression | foreachExpression
  lazy val expression: Parser[Expression] = rule(blockExpression | ifExpression | whileExpression | comparable)

  //ifExpression ::= "if" "(" expression ")" expression "else" expression
  lazy val ifExpression: Parser[Expression] = rule {
    (IF << LPAREN) ~ (expression ~ RPAREN ~ expression ~ ELSE ~ expression) ^^ {
      case _ ~ (condition ~ _ ~ positive ~ _ ~ negative) => If(condition, positive, negative)
    }
  }

  //whileExpression ::= "while" "(" expression ")" expression
  lazy val whileExpression: Parser[Expression] = rule {
    (WHILE << LPAREN) ~ (expression ~ RPAREN ~ expression) ^^ {
      case _ ~ (condition ~ _ ~ body) => While(condition, body)
    }
  }

  //blockExpression ::= "{" line+ "}"
  lazy val blockExpression: Parser[Expression] = rule {
    for {
      _ <- LBRACE
      elements <- line.+
      _ <- RBRACE
    } yield Sequence(elements)
  }

  //comparable ::= add {"==" add | "<=" add | "=>" add | "<" add | ">" add}
  lazy val comparable: Parser[Expression] = rule {
    chainl(add)(
      (EQEQ  ^^ { _ => (left: Expression, right: Expression) => BinaryExpression(Operator.Eq, left, right) })
    | (LTE ^^ { _ => (left: Expression, right: Expression) => BinaryExpression(Operator.Lte, left, right) })
    | (GTE ^^ { _ => (left: Expression, right: Expression) => BinaryExpression(Operator.Gte, left, right) })
    | (LT  ^^ { _ => (left: Expression, right: Expression) => BinaryExpression(Operator.Lt, left, right) })
    | (GT  ^^ { _ => (left: Expression, right: Expression) => BinaryExpression(Operator.Gt, left, right) })
    )
  }

  //add ::= term {"+" term | "-" term}
  lazy val add: Parser[Expression] = rule {
    chainl(term)(
      (PLUS) ^^  { case _ => (left: Ast.Expression, right: Ast.Expression) => BinaryExpression(Operator.Add, left, right) } |
      (MINUS) ^^ { case _ => (left: Ast.Expression, right: Ast.Expression) => BinaryExpression(Operator.Subtract, left, right) }
    )
  }

  //term ::= factor {"*" factor | "/" factor}
  lazy val term: Parser[Expression] = rule {
    chainl(primary)(
      (ASTER) ^^ { case _ => (left: Ast.Expression, right: Ast.Expression) => BinaryExpression(Operator.Multiply, left, right) }
    | (SLASH) ^^ { case _ => (left: Ast.Expression, right: Ast.Expression) => BinaryExpression(Operator.Divide, left, right) }
    )
  }

  //primary ::= "(" expression ")" | integerLiteral | identifier
  lazy val primary: Parser[Expression] = rule {
    (LPAREN >> expression << RPAREN) | integerLiteral | identifier
  }

  //integerLiteral ::= ["1"-"9"] {["0"-"9"]}
  lazy val integerLiteral: Parser[Expression] = (("""[1-9][0-9]*|0""").r ^^ {
    case text => Literal(Integer.parseInt(text))
  }) << SPACING

  //identifier ::= ["a"-"z","A"-"Z"]{["a"-"z","A"-"Z","0"-"9","_"]}
  lazy val identifier: Parser[Id] = ID ^^ {
    case text => Id(text)
  }

  def parseAll(input: String): Program = {
    parse(program, input) match {
      case Result.Success(program) => program
      case Result.Failure(_, message) => throw new InterpreterException(message)
    }
  }
}
