package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast.types.Tables
import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

object BlockStatement {
  def emptyBlock: BlockStatement = BlockStatement(-1, List())
}

case class BlockStatement(
    lineNum: Int,
    statements: List[Statement]
) extends Statement {

  def semanticCheck(tables: Tables, function: Function): Boolean = {
    statements.map(_.semanticCheck(tables, function)).exists(identity)
  }

  def interp(state: State): Option[MiniValues] = {
    statements.foreach(Statement.interp(_, state).foreach(v => return Some(v)))
    None
  }

  def gatherTerms: Set[MiniAST] = statements.flatMap(_.gatherTerms).toSet + this
}
