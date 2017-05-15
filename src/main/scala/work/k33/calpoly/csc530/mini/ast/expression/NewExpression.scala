package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.StructS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.types.{StructType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State, Struct}

import scala.collection.mutable

case class NewExpression(lineNum: Int, id: String) extends Expression {
  def typeCheck(tables: Tables): Type = {
    if (tables.structTable.contains(id)) {
      StructType(lineNum, id)
    } else {
      throw SemanticCheckException(lineNum, s"Unknown struct $id")
    }
  }

  def interp(state: State): MiniValues = {
    MiniValues(Struct(mutable.Map(), false), StructS(mutable.Map()))
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}
