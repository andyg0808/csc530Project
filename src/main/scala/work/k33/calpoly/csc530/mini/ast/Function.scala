package work.k33.calpoly.csc530.mini.ast

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini._
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.statement.{BlockStatement, Statement}
import work.k33.calpoly.csc530.mini.ast.types._
import work.k33.calpoly.csc530.mini.interp.{MiniValues, State, VoidValue}

import scala.collection.mutable

case class Function(
    lineNum: Int,
    name: String,
    params: List[Declaration],
    retType: Type,
    locals: List[Declaration],
    body: BlockStatement) extends MiniAST {

  def semanticCheck(tables: Tables): Unit = {
    if (hasDuplicates(params.map(_.name) ++ locals.map(_.name))) {
      throw SemanticCheckException(lineNum, s"Function $name has duplicate params/locals")
    }

    validateType(tables.structTable, retType, lineNum)
    params.map(_.typ).foreach(validateType(tables.structTable, _, lineNum))
    locals.map(_.typ).foreach(validateType(tables.structTable, _, lineNum))

    val newTables = updateSymbolTable(tables)
    val bodyCheck = body.semanticCheck(newTables, this)
    if (!(retType matches VoidType()) && !bodyCheck) {
      throw SemanticCheckException(lineNum, s"Function $name does not return along all paths")
    }
  }

  def interp(oldState: State, args: List[MiniValues]): MiniValues = {
    oldState.coverage += this
    val localState = mutable.Map() ++ locals.map(decl => decl.name -> None) ++ params.map(_.name).zip(args.map(Some(_)))
    val newState = oldState.copy(locals = localState)
    Statement.interp(body, newState).getOrElse(MiniValues(VoidValue(), NullS())) // dummy symbolic value
  }

  def gatherTerms: Set[MiniAST] = body.gatherTerms + this

  private def updateSymbolTable(tables: Tables): Tables = {
    val updatedSymbolTable = tables.symbolTable ++
      params.map(param => param.name -> types.SymbolId(param.typ, Scope.PARAM)).toMap ++
      locals.map(local => local.name -> types.SymbolId(local.typ, Scope.LOCAL)).toMap

    Tables(updatedSymbolTable, tables.structTable, tables.funcTable)
  }

  def isVoid: Boolean = retType matches VoidType()
}