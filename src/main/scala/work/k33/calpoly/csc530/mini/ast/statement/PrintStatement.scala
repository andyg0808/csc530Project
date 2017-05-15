package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{IntType, Tables}
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

case class PrintStatement(lineNum: Int, expression: Expression) extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    expression.typeCheck(tables) match {
      case IntType() => false
      case _ => throw SemanticCheckException(lineNum, s"Can't delete non-struct")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    print(s"${Expression.interp(expression, state).concrete} ")
    None
  }

  def gatherTerms: Set[MiniAST] = expression.gatherTerms + this
}