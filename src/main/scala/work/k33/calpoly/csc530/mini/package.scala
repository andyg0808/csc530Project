package work.k33.calpoly.csc530

import work.k33.calpoly.csc530.mini.ast.exception.SemanticCheckException
import work.k33.calpoly.csc530.mini.ast.types.{Declaration, StructType, Type}

package object mini {
  def hasDuplicates[T](list: Traversable[T]): Boolean = {
    list.toSet.size != list.size
  }

  def validateType(typeTable: Map[String, List[Declaration]], typ: Type, lineNum: Int): Unit = {
    typ match {
      case StructType(_, name) =>
        if (!typeTable.contains(name)) throw SemanticCheckException(lineNum, s"Invalid struct '$name'")
      case _ =>
    }
  }
}
