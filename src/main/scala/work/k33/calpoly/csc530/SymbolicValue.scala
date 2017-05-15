package work.k33.calpoly.csc530

import scala.collection.mutable

trait SymbolicValue
trait SymbolicNum extends SymbolicValue
trait SymbolicBool extends SymbolicValue

case class IdS(index: Int) extends SymbolicNum
case class NumS(num: Long) extends SymbolicNum
case class ArithS(op: Symbol, left: SymbolicNum, right: SymbolicNum) extends SymbolicNum

case class BoolS(bool: Boolean) extends SymbolicBool
case class CmpS(op: Symbol, left: SymbolicNum, right: SymbolicNum) extends SymbolicBool
case class LogicS(op: Symbol, left: SymbolicBool, right: SymbolicBool) extends SymbolicBool
case class NotS(boolSym: SymbolicBool) extends SymbolicBool

case class StructS(fields: mutable.Map[String, SymbolicValue]) extends SymbolicValue

case class FuncS() extends SymbolicValue

case class NullS() extends SymbolicValue