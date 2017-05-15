package work.k33.calpoly.csc530.mini.interp

import work.k33.calpoly.csc530.StructS

trait MiniLValue

case class MiniLValueId(id: String) extends MiniLValue

case class MiniLValueDot(struct: Struct, symStruct: StructS, field: String) extends MiniLValue
