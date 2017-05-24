package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530._
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.UnaryOperator._
import work.k33.calpoly.csc530.mini.ast.types.{BoolType, IntType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp._

object UnaryExpression {
  final val OP_MAP = Map("!" -> NOT, "-" -> MINUS)

  def create(lineNum: Int, opStr: String, operand: Expression): UnaryExpression = {
    new UnaryExpression(lineNum, OP_MAP(opStr), operand)
  }
}

class UnaryExpression private(val lineNum: Int, val operator: UnaryOperator, val operand: Expression)
  extends Expression {
  def typeCheck(tables: Tables): Type = {
    val operandType = operand.typeCheck(tables)

    operator match {
      case NOT =>
        if (operandType matches BoolType())
          BoolType()
        else
          throw SemanticCheckException(lineNum, s"Not operator expects boolean operand, got $operandType")

      case MINUS =>
        if (operandType matches IntType())
          IntType()
        else
          throw SemanticCheckException(lineNum, s"Unary minus operator expects int operand, got $operandType")
    }
  }

  def interp(state: State): MiniValues = {
    (operator, Expression.interp(operand, state)) match {
      case (MINUS, MiniValues(Num(x), ArithS('-, NumS(0), s: SymbolicNum))) => MiniValues(Num(-x), s)
      case (MINUS, MiniValues(Num(x), NumS(s))) => MiniValues(Num(-x), NumS(-s))
      case (MINUS, MiniValues(Num(x), s: SymbolicNum)) => MiniValues(Num(-x), ArithS('-, NumS(0), s))
      case (NOT, MiniValues(Bool(x), NotS(s: SymbolicBool))) => MiniValues(Bool(!x), s)
      case (NOT, MiniValues(Bool(x), s: SymbolicBool)) => MiniValues(Bool(!x), NotS(s))
    }
  }

  def gatherTerms: Set[MiniAST] = operand.gatherTerms + this
}