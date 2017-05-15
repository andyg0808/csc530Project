package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.StructS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.{MiniInterpreterException, SemanticCheckException}
import work.k33.calpoly.csc530.mini.ast.types.{StructType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp._

case class DotExpression(lineNum: Int, left: Expression, id: String) extends Expression {
  var leftType: StructType = _
  def typeCheck(tables: Tables): Type = {
    left.typeCheck(tables) match {
      case struct @ StructType(_, name) =>
        leftType = struct
        tables.structTable(name).find(_.name == id).map(_.typ).getOrElse(throw SemanticCheckException(lineNum, s"struct $name doesn't have field $id"))
      case _ => throw SemanticCheckException(lineNum, s"Tried to access field of $leftType")
    }
  }

  def interp(state: State): MiniValues = {
    Expression.interp(left, state) match {
      case MiniValues(Struct(fields, freed), StructS(symFields)) =>
        if (freed)
          throw MiniInterpreterException(lineNum, "Accessing field from freed struct")
        MiniValues(fields(id), symFields(id))
      case MiniValues(NullValue(), _) => throw MiniInterpreterException(lineNum, "Dereferencing null")
    }
  }

  def gatherTerms: Set[MiniAST] = left.gatherTerms + this
}
