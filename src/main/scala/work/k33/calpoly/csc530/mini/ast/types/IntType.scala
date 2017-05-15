package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.NumS
import work.k33.calpoly.csc530.mini.interp.{MiniValues, Num}

case class IntType() extends Type {
  def matches(otherType: Type): Boolean = {
    otherType.isInstanceOf[IntType]
  }

  def defaultValues: MiniValues = MiniValues(Num(0), NumS(0))

  override def toString: String = "int"
}