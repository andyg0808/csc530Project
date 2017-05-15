package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.types.{Tables, Type}
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State}

case class InvocationExpression(
    lineNum: Int,
    name: String,
    arguments: List[Expression]
) extends Expression {
  def typeCheck(tables: Tables): Type = {
    tables.funcTable.get(name).map(func => {
      val paramTypes = func.params.map(_.typ)
      val argTypes = arguments.map(_.typeCheck(tables))
      if (paramTypes.size == argTypes.size && paramTypes.zip(argTypes).forall({ case (paramType, argType) => paramType matches argType })) {
        func.retType
      } else {
        throw SemanticCheckException(lineNum, s"You gave function $name bad args")
      }
    }).getOrElse(throw SemanticCheckException(lineNum, s"Function $name does not exist"))
  }

  def interp(state: State): MiniValues = {
    state.functions(name).interp(state, arguments.map(Expression.interp(_, state)))
  }

  def gatherTerms: Set[MiniAST] = arguments.flatMap(_.gatherTerms).toSet + this
}
