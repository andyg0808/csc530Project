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

object NotS {
  /**
    * This is fierce. I'm overriding the default apply() for a case class to allow me to optimize
    * away object creation in some places.
    * This is the easiest way to handle all places a NotS might be created.
    */
  def apply(boolSym: SymbolicBool): SymbolicBool = {
    boolSym match {
      case BoolS(b) => BoolS(!b) // This and the next case are the reasons we can't do this easily anywhere but in a factory method.
      case NotS(a: SymbolicBool) => a
      case _ => new NotS(boolSym)
    }
  }
}

object CmpS {
  /**
    * As with NotS, this overrides the case class's default apply() to allow CmpSs to be optimized
    * away.
    */
  def apply(op: Symbol, left: SymbolicNum, right: SymbolicNum): SymbolicBool = {
    (left, right) match {
      case (NumS(l), NumS(r)) => op match {
        case '== => BoolS(l == r)
        case '!= => BoolS(l != r)
        case '< => BoolS(l < r)
        case '<= => BoolS(l <= r)
        case '> => BoolS(l > r)
        case '>= => BoolS(l >= r)
      }
      case _ => new CmpS(op, left, right)
    }
  }
}

case class StructS(fields: mutable.Map[String, SymbolicValue]) extends SymbolicValue

case class FuncS() extends SymbolicValue

case class NullS() extends SymbolicValue