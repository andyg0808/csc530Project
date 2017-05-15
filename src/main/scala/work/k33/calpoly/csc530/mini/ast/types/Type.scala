package work.k33.calpoly.csc530.mini.ast.types

import work.k33.calpoly.csc530.mini.interp.MiniValues

trait Type {
  def matches(otherType: Type): Boolean
  def defaultValues: MiniValues
}