package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.types.{Tables, VoidType}
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State, VoidValue}

case class ReturnEmptyStatement(lineNum: Int) extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    function.retType match {
      case VoidType() => true
      case _ => throw SemanticCheckException(lineNum, s"Empty return in non-void function")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    Some(MiniValues(VoidValue(), NullS())) // dummy symbolic value
  }

  def gatherTerms: Set[MiniAST] = Set(this)
}