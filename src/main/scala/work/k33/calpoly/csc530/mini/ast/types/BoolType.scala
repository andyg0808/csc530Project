package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.BoolS
import work.k33.calpoly.csc530.mini.interp.{Bool, MiniValues}

case class BoolType() extends Type {
  def matches(otherType: Type): Boolean = {
    otherType.isInstanceOf[BoolType]
  }

  def defaultValues: MiniValues = MiniValues(Bool(false), BoolS(false))

  override def toString: String = "bool"
}