package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.{MiniInterpreterException, SemanticCheckException}
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

case class IdentifierExpression(lineNum: Int, id: String) extends Expression {
  def typeCheck(tables: Tables): Type = {
    tables.symbolTable.getOrElse(id, throw SemanticCheckException(lineNum, s"$id is not in scope")).typ
  }

  def interp(state: State): MiniValues = {
    state.locals.getOrElse(id, Some(state.globals(id)))
        .getOrElse(throw MiniInterpreterException(lineNum, s"Use of uninitialized value $id"))
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}
