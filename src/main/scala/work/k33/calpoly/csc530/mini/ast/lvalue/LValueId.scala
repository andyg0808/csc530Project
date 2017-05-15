package work.k33.calpoly.csc530.mini.ast.lvalue

import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniLValue, MiniLValueId, State}

case class LValueId(lineNum: Int, id: String) extends LValue {
  def typeCheck(tables: Tables): Type = {
    tables.symbolTable.getOrElse(id, throw SemanticCheckException(lineNum, s"$id is not in scope")).typ
  }

  def interp(state: State): MiniLValue = {
    MiniLValueId(id)
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}