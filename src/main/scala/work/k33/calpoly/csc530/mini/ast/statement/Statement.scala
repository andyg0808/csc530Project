package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast.types.Tables
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

object Statement {
  def interp(statement: Statement, state: State): Option[MiniValues] = {
    state.coverage += statement
    statement.interp(state)
  }
}

trait Statement extends MiniAST {
  def semanticCheck(tables: Tables, function: Function): Boolean
  def interp(state: State): Option[MiniValues]
  def lineNum: Int
}