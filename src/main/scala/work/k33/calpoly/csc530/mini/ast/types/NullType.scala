package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini.interp.{MiniValues, NullValue}

case class NullType() extends Type {
  def matches(otherType: Type): Boolean = {
    otherType.isInstanceOf[NullType] || otherType.isInstanceOf[StructType]
  }

  def defaultValues: MiniValues = MiniValues(NullValue(), NullS()) // dummy values
}
