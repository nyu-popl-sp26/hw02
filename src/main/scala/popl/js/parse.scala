package popl.js

import popl.js.util.JsException
import ast._
import Expr._, Bop._, Uop._

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.StreamReader

object parse extends JavaTokenParsers:
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  val reserved: Set[String] =
    Set("undefined", "true", "false", "null",
      "const", "var", "name", "ref",
      "function", "return", "interface",
      "bool", "string", "number", "Undefined", "Null")

  def stmt: Parser[Expr] = basicStmt

  def stmtSep: Parser[String] = ";"

  def basicStmt: Parser[Expr] =
    expr <~ opt(stmtSep)

  def expr: Parser[Expr] = additiveExpr

  def additiveOp: Parser[Bop] =
    "+" ^^^ Plus |
      "-" ^^^ Minus

  def additiveExpr: Parser[Expr] =
    multitiveExpr ~ rep(additiveOp ~ multitiveExpr) ^^ { case e1 ~ opes =>
      opes.foldLeft(e1) { case (e1, op ~ e2) => BinOp(op, e1, e2).setPos(e1.pos) }
    }

  def multitiveOp: Parser[Bop] =
    "*" ^^^ Times |
      "/" ^^^ Div

  def multitiveExpr: Parser[Expr] =
    unaryExpr ~ rep(multitiveOp ~ unaryExpr) ^^ { case e1 ~ opes =>
      opes.foldLeft(e1) { case (e1, op ~ e2) => BinOp(op, e1, e2).setPos(e1.pos) }
    }

  def unaryOp: Parser[Uop] =
    "-" ^^^ UMinus

  def unaryExpr: Parser[Expr] =
    positioned(unaryOp ~ primaryExpr ^^ { case uop ~ e => UnOp(uop, e) }) |
      primaryExpr

  def primaryExpr: Parser[Expr] =
    literalExpr |
      "(" ~> expr <~ ")"

  def literalExpr: Parser[Expr] =
    positioned(floatingPointNumber ^^ { d => Num(d.toDouble) })

  /** utility functions */
  private def getExpr(p: ParseResult[Expr]): Expr =
    p match {
      case Success(e, _) => e

      case p: NoSuccess =>
        throw new JsException(p.msg, p.next.pos)
    }

  def fromString(s: String): Expr = getExpr(parseAll(stmt, s))

  def fromFile(file: java.io.File): Expr =
    val reader = new java.io.FileReader(file)
    val result = parseAll(stmt, StreamReader(reader))
    getExpr(result)
