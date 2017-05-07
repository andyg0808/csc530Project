package work.k33.calpoly.csc530.pact

trait SymbolicValue
trait SymbolicNum extends SymbolicValue
trait SymbolicBool extends SymbolicValue

case class IdS(index: Int) extends SymbolicNum
case class NumS(num: Int) extends SymbolicNum
case class ArithS(op: Symbol, left: SymbolicNum, right: SymbolicNum) extends SymbolicNum

case class BoolS(bool: Boolean) extends SymbolicBool
case class CmpS(op: Symbol, left: SymbolicNum, right: SymbolicNum) extends SymbolicBool
case class LogicS(op: Symbol, left: SymbolicBool, right: SymbolicBool) extends SymbolicBool
case class NotS(boolSym: SymbolicBool) extends SymbolicBool
case class FuncS() extends SymbolicValue