package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast._
import work.k33.calpoly.csc530.mini.ast.exception.{MiniInterpreterException, SemanticCheckException}
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.types.{StructType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, NullValue, State, Struct}

case class DeleteStatement(lineNum: Int, expression: Expression) extends Statement {
  var structType: Type = _
  def semanticCheck(tables: Tables, function: Function): Boolean = {
    structType = expression.typeCheck(tables)
    structType match {
      case StructType(_, _) => false
      case _ => throw SemanticCheckException(lineNum, s"Can't delete non-struct")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    Expression.interp(expression, state) match {
      case MiniValues(s: Struct, _) =>
        if (s.freed) {
          throw MiniInterpreterException(lineNum, "Double free")
        }
        else {
          s.freed = true
        }
      case MiniValues(NullValue(), _) => // freeing null is a no-op
    }
    None
  }

  def gatherTerms: Set[MiniAST] = expression.gatherTerms + this
}

