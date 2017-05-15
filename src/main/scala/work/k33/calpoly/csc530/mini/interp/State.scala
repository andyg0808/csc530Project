package work.k33.calpoly.csc530.mini.interp

import work.k33.calpoly.csc530.mini.ast.{Function, MiniAST}
import work.k33.calpoly.csc530.{IdS, InputProvider, SymbolicBool}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class State(
    globals: mutable.Map[String, MiniValues],
    locals: mutable.Map[String, Option[MiniValues]], // value is None if uninitialized
    functions: Map[String, Function],
    constraints: ArrayBuffer[SymbolicBool],
    inputProvider: InputProvider,
    inputs: ArrayBuffer[Int],
    coverage: mutable.Set[MiniAST],
    symbols: mutable.ArrayBuffer[IdS]
)
