package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.IdS
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.types.{IntType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, Num, State}

case class ReadExpression(lineNum: Int) extends Expression {
  def typeCheck(stables: Tables): Type = {
    IntType()
  }

  def interp(state: State): MiniValues = {
    val symbol = IdS(state.symbols.lastOption.map(_.index + 1).getOrElse(0))
    state.symbols += symbol
    val input = state.inputProvider.readInt()
    state.inputs += input
    MiniValues(Num(input), symbol)
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}
