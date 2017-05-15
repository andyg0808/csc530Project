package work.k33.calpoly.csc530.mini.ast.lvalue

import work.k33.calpoly.csc530.StructS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{StructType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp._

case class LValueDot(lineNum: Int, left: Expression, id: String) extends LValue {
  var leftType: StructType = _

  def typeCheck(tables: Tables): Type = {
    left.typeCheck(tables) match {
      case struct@StructType(_, name) =>
        leftType = struct
        tables.structTable(name).find(_.name == id).map(_.typ).getOrElse(throw SemanticCheckException(lineNum, s"struct $name doesn't have field $id"))
      case _ => throw SemanticCheckException(lineNum, s"Tried to do a dot on a thing which isn't a struct")
    }
  }

  def interp(state: State): MiniLValue = {
    Expression.interp(left, state) match {
      case MiniValues(concrete: Struct, symbolic: StructS) => MiniLValueDot(concrete, symbolic, id)
    }
  }

  def gatherTerms: Set[MiniAST] = left.gatherTerms + this
}