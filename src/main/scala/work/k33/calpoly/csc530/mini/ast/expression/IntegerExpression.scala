package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.NumS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.types.{IntType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, Num, State}

case class IntegerExpression(lineNum: Int, value: String) extends Expression {
  def typeCheck(tables: Tables): Type = {
    IntType()
  }

  def interp(state: State): MiniValues = {
    MiniValues(Num(value.toLong), NumS(value.toLong))
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}
