package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini.interp.{MiniValues, NullValue}

case class StructType(lineNum: Int, name: String) extends Type {
  def matches(otherType: Type): Boolean = {
    otherType match {
      case NullType() => true
      case StructType(_, otherName) => name == otherName
      case _ => false
    }
  }

  def defaultValues: MiniValues = MiniValues(NullValue(), NullS())

  override def toString: String = s"struct $name"
}