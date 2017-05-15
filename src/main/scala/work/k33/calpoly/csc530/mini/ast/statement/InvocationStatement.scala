package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.Tables
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

case class InvocationStatement(lineNum: Int, expression: Expression) extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    expression.typeCheck(tables)
    false
  }

  def interp(state: State): Option[MiniValues] = {
    Expression.interp(expression, state)
    None
  }

  def gatherTerms: Set[MiniAST] = expression.gatherTerms + this
}