package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.{NotS, SymbolicBool}
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{BoolType, Tables}
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{Bool, MiniValues, State}

case class WhileStatement(lineNum: Int, guard: Expression, body: Statement) extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    if (guard.typeCheck(tables) matches BoolType()) {
      body.semanticCheck(tables, function)
      false
    } else {
      throw SemanticCheckException(lineNum, "Guard is not boolean")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    var cond = Expression.interp(guard, state)
    while (cond.concrete.asInstanceOf[Bool].bool) {
      state.constraints += cond.symbolic.asInstanceOf[SymbolicBool]
      Statement.interp(body, state).foreach(x => return Some(x))
      cond = Expression.interp(guard, state)
    }
    state.constraints += NotS(cond.symbolic.asInstanceOf[SymbolicBool])
    None
  }

  def gatherTerms: Set[MiniAST] = Set(guard, body).flatMap(_.gatherTerms) + this
}