package work.k33.calpoly.csc530.mini.ast.statement

import work.k33.calpoly.csc530.mini.ast._
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.expression.Expression
import work.k33.calpoly.csc530.mini.ast.lvalue.LValue
import work.k33.calpoly.csc530.mini.ast.types.Tables
import work.k33.calpoly.csc530.mini.interp._

case class AssignmentStatement(lineNum: Int, target: LValue, source: Expression) extends Statement {
  def semanticCheck(tables: Tables, function: Function): Boolean = {
    val targetType = target.typeCheck(tables)
    val sourceType = source.typeCheck(tables)
    if (targetType matches sourceType) {
      false
    } else {
      throw SemanticCheckException(lineNum, s"Can't assign $sourceType into $targetType")
    }
  }

  def interp(state: State): Option[MiniValues] = {
    val sourceVal = Expression.interp(source, state)
    LValue.interp(target, state) match {
      case MiniLValueId(id) =>
        if (state.locals contains id)
          state.locals.put(id, Some(sourceVal))
        else
          state.globals.put(id, sourceVal)
      case MiniLValueDot(struct, symStruct, field) =>
        struct.fields.put(field, sourceVal.concrete)
        symStruct.fields.put(field, sourceVal.symbolic)
    }
    None
  }

  def gatherTerms: Set[MiniAST] = Set(target, source).flatMap(_.gatherTerms) + this
}