package work.k33.calpoly.csc530.pact

sealed trait ExprC

case class NumC(num: Int) extends ExprC
case class IdC(sym: Symbol) extends ExprC
case class IfC(guard: ExprC, ifTrue: ExprC, ifFalse: ExprC) extends ExprC
case class AppC(func: ExprC, args: List[ExprC]) extends ExprC
case class LamC(params: List[Symbol], body: ExprC) extends ExprC
