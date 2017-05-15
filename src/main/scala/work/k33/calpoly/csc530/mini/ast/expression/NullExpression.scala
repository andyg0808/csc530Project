package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.types.{NullType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, NullValue, State}

case class NullExpression(lineNum: Int) extends Expression {
  def typeCheck(tables: Tables): Type = {
    NullType()
  }

  def interp(state: State): MiniValues = MiniValues(NullValue(), NullS())

  def gatherTerms: Set[MiniAST] = Set(this)
}
