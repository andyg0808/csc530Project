package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

case class ReturnStatement(lineNum: Int, expression: Expression) extends Statement {
  var retType: Type = _

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    retType = function.retType
    val expType = expression.typeCheck(tables)
    if (expType matches retType) {
      true
    } else {
      throw SemanticCheckException(lineNum, s"Function ${function.name} returns $retType, but you returned $expType")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    Some(Expression.interp(expression, state))
  }

  def gatherTerms: Set[MiniAST] = expression.gatherTerms + this
}