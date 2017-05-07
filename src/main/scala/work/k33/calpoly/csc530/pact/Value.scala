package work.k33.calpoly.csc530.pact

import work.k33.calpoly.csc530.pact.PactInterpreter.{Env, SymbolicEnv}

sealed trait Value

case class NumV(num: Int) extends Value
case class BoolV(bool: Boolean) extends Value
case class CloV(params: List[Symbol], body: ExprC, env: Env, symEnv: SymbolicEnv) extends Value
case class PrimV(proc: List[Values] => Values) extends Value