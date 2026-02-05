package popl.js

import scala.util.parsing.input.Positional

object ast:
  /* JakartaScript Expressions */
  enum Expr extends Positional:
    // pretty print as AST
    override def toString: String = print.prettyAST(this)

    // Pretty print as JavaScript expression
    def prettyJS: String = print.prettyJS(this)

    // Pretty print expression as value
    def prettyVal: String = print.prettyVal(this)

    // Literals
    case Num(n: Double)

    // Unary and binary operator expressions
    case UnOp(op: Uop, e1: Expr)
    case BinOp(op: Bop, e1: Expr, e2: Expr)
  end Expr

  /* Unary operators */
  enum Uop:
    case UMinus /* - */

  /* Binary operators */
  enum Bop:
    case Plus, Minus, Times, Div /* + - * / */

  /* Values */
  type Val = Expr.Num

  /* Define values. */
  def isValue(e: Expr): Boolean = e match
    case _: Val => true
    case _ => false

  /* Pretty-print values. */
  def pretty(v: Val): String =
    v match
      case Expr.Num(n) => n.toString
