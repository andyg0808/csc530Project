package work.k33.calpoly.csc530.pact

sealed trait Value

case class NumV(num: Int) extends Value
case class BoolV(bool: Boolean) extends Value
case class CloV(params: List[Symbol], body: ExprC, env: Map[Symbol, Value]) extends Value
case class PrimV(proc: List[Value] => Value) extends Value