package work.k33.calpoly.csc530.mini.ast.lvalue

import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniLValue, State}

object LValue {
  def interp(lvalue: LValue, state: State): MiniLValue = {
    state.coverage += lvalue
    lvalue.interp(state)
  }
}

trait LValue extends MiniAST {
  def typeCheck(tables: Tables): Type
  def interp(state: State): MiniLValue
}