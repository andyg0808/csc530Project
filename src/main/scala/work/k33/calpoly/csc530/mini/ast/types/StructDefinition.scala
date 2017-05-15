package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.mini._
import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException

case class StructDefinition(lineNum: Int, name: String, fields: List[Declaration]) {
  def semanticCheck(typeTable: Map[String, List[Declaration]]): Unit = {
    if (hasDuplicates(fields.map(_.name))) {
      throw SemanticCheckException(lineNum, s"struct $name has duplicate fields")
    }
    fields.map(_.typ).foreach(validateType(typeTable, _, lineNum))
  }
}