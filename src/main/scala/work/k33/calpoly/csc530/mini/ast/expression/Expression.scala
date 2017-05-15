package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

object Expression {
  def interp(expr: Expression, state: State): MiniValues = {
    state.coverage += expr
    expr.interp(state)
  }
}

trait Expression extends MiniAST {
  def typeCheck(tables: Tables): Type
  def interp(state: State): MiniValues
}
