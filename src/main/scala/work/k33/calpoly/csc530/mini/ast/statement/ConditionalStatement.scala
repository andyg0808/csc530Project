package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.{NotS, SymbolicBool}
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{BoolType, Tables}
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{Bool, MiniValues, State}

case class ConditionalStatement(lineNum: Int, guard: Expression, thenBlock: Statement, elseBlock: Statement)
  extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    if (guard.typeCheck(tables) matches BoolType()) {
      val thenCheck = thenBlock.semanticCheck(tables, function)
      val elseCheck = elseBlock.semanticCheck(tables, function)
      thenCheck && elseCheck
    } else {
      throw SemanticCheckException(lineNum, s"Guard isn't a boolean")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    Expression.interp(guard, state) match {
      case MiniValues(Bool(b), cond: SymbolicBool) =>
        if (b) {
          state.constraints += cond
          Statement.interp(thenBlock, state)
        } else {
          state.constraints += NotS(cond)
          Statement.interp(elseBlock, state)
        }
    }
  }

  def gatherTerms: Set[MiniAST] = Set(guard, thenBlock, elseBlock).flatMap(_.gatherTerms) + this
}
