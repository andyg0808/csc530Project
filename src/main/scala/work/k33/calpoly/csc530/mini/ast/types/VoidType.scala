package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.NullS
import work.k33.calpoly.csc530.mini.interp.{MiniValues, VoidValue}

case class VoidType() extends Type {
  def matches(otherType: Type): Boolean = {
    otherType.isInstanceOf[VoidType]
  }

  def defaultValues: MiniValues = MiniValues(VoidValue(), NullS()) // dummy values
}