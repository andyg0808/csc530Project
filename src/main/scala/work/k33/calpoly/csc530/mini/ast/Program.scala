package work.k33.calpoly.csc530.mini.ast

import work.k33.calpoly.csc530.{InputProvider, Result}
import work.k33.calpoly.csc530.mini._
import work.k33.calpoly.csc530.mini.ast.exception.{MiniInterpreterException, SemanticCheckException}
import work.k33.calpoly.csc530.mini.ast.types._
import work.k33.calpoly.csc530.mini.interp.{MiniValues, Num, State}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Program(
    structs: List[StructDefinition],
    globals: List[Declaration],
    funcs: List[Function]
) extends MiniAST {
  def semanticCheck(): Unit = {
    val tables = makeTables()

    if (hasDuplicates(structs.map(_.name))) {
      throw SemanticCheckException(1, "Two structs have the same name")
    }
    if (hasDuplicates(globals.map(_.name))) {
      throw SemanticCheckException(1, "Two globals have the same name")
    }
    if (hasDuplicates(funcs.map(_.name))) {
      throw SemanticCheckException(1, "Two functions have the same name")
    }
    if (!funcs.exists(func => func.name == "main" && func.retType.matches(IntType()) && func.params.isEmpty)) {
      throw SemanticCheckException(1, "No main function")
    }

    globals.map(_.typ).foreach(validateType(tables.structTable, _, 0))
    structs.foreach(_.semanticCheck(tables.structTable))
    funcs.foreach(_.semanticCheck(tables))
  }

  def makeTables(): Tables = {
    val symbolTable = globals.map(decl => decl.name -> SymbolId(decl.typ, Scope.GLOBAL)).toMap
    val typeTable = structs.map(typ => typ.name -> typ.fields).toMap
    val funcTable = funcs.map(func => func.name -> func).toMap
    Tables(symbolTable, typeTable, funcTable)
  }

  def interp(inputProvider: InputProvider): Result[MiniAST] = {
    val state = State(
      mutable.Map() ++ globals.map(decl => decl.name -> decl.typ.defaultValues),
      mutable.Map(),
      funcs.map(func => func.name -> func).toMap,
      ArrayBuffer(),
      inputProvider,
      ArrayBuffer(),
      mutable.Set(this),
      ArrayBuffer())

    val result: Either[String, String] =
      try {
        val MiniValues(Num(returnCode), _) = funcs.find(_.name == "main").get.interp(state, List())
        Right(returnCode.toString)
      } catch {
        case e: MiniInterpreterException => Left(s"Error on $e")
      }

    Result(
      result,
      state.constraints.reverseIterator.toList,
      state.symbols.size,
      state.coverage.toSet,
      state.inputs.toList)
  }

  def gatherTerms: Set[MiniAST] = funcs.flatMap(_.gatherTerms).toSet + this

  def lineNum: Int = -1
}