package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.mini.ast.Function

case class Tables(
    symbolTable: Map[String, SymbolId],
    structTable: Map[String, List[Declaration]],
    funcTable: Map[String, Function]
)