package work.k33.calpoly.csc530.mini.ast.expression

import work.k33.calpoly.csc530._
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.exception.{MiniInterpreterException, SemanticCheckException}
import work.k33.calpoly.csc530.mini.ast.expression.BinaryExpression._
import work.k33.calpoly.csc530.mini.ast.expression.BinaryOperator._
import work.k33.calpoly.csc530.mini.ast.types.{BoolType, IntType, Tables, Type}
import work.k33.calpoly.csc530.mini.interp._

object BinaryExpression {
  final val OP_MAP: Map[String, BinaryOperator] = BinaryOperator.values().map(op => op.opStr -> op).toMap

  private def arith(f: (Long, Long) => Long)(l: MiniValue, r: MiniValue): MiniValue = {
    (l, r) match {
      case (Num(a), Num(b)) => Num(f(a, b))
    }
  }

  private def cmp(f: (Long, Long) => Boolean)(l: MiniValue, r: MiniValue): MiniValue = {
    (l, r) match {
      case (Num(a), Num(b)) => Bool(f(a, b))
    }
  }

  private def logical(f: (Boolean, Boolean) => Boolean)(l: MiniValue, r: MiniValue): MiniValue = {
    (l, r) match {
      case (Bool(a), Bool(b)) => Bool(f(a, b))
    }
  }

  private def miniEq(l: MiniValue, r: MiniValue): MiniValue = {
    (l, r) match {
      case (Num(a), Num(b)) => Bool(a == b)
      case (a: Struct, b: Struct) => Bool(a eq b) // Hopefully this works
      case (Struct(_, _), NullValue()) => Bool(false)
      case (NullValue(), Struct(_, _)) => Bool(false)
      case (NullValue(), NullValue()) => Bool(true)
    }
  }

  private def miniNe(l: MiniValue, r: MiniValue): MiniValue = {
    Bool(!miniEq(l, r).asInstanceOf[Bool].bool)
  }

  private def arithSym(op: Symbol)(l: SymbolicValue, r: SymbolicValue): SymbolicValue = {
    (l, r) match {
      case (a: SymbolicNum, b: SymbolicNum) => ArithS(op, a, b)
    }
  }

  private def cmpSym(op: Symbol)(l: SymbolicValue, r: SymbolicValue): SymbolicValue = {
    (l, r) match {
      case (a: SymbolicNum, b: SymbolicNum) => CmpS(op, a, b)
    }
  }

  private def logicalSym(op: Symbol)(l: SymbolicValue, r: SymbolicValue): SymbolicValue = {
    (l, r) match {
      case (a: SymbolicBool, b: SymbolicBool) => LogicS(op, a, b)
    }
  }

  private def miniEqSym(l: SymbolicValue, r: SymbolicValue): SymbolicBool = {
    (l, r) match {
      case (a: SymbolicNum, b: SymbolicNum) => CmpS('==, a, b)
      case (a: StructS, b: StructS) => BoolS(a eq b)
      case (_: StructS, NullS()) => BoolS(false)
      case (NullS(), _: StructS) => BoolS(false)
      case (NullS(), NullS()) => BoolS(true)
    }
  }

  private def miniNeSym(l: SymbolicValue, r: SymbolicValue): SymbolicBool = {
    NotS(miniEqSym(l, r))
  }

  final val INTERP_MAP: Map[BinaryOperator, (MiniValue, MiniValue) => MiniValue] = Map(
    TIMES -> arith(_ * _),
    DIVIDE -> arith(_ / _),
    PLUS -> arith(_ + _),
    MINUS -> arith(_ - _),
    AND -> logical(_ && _),
    OR -> logical(_ || _),
    LT -> cmp(_ < _),
    LE -> cmp(_ <= _),
    GT -> cmp(_ > _),
    GE -> cmp(_ >= _),
    EQ -> miniEq,
    NE -> miniNe
  )

  final val INTERP_SYM_MAP: Map[BinaryOperator, (SymbolicValue, SymbolicValue) => SymbolicValue] = Map(
    TIMES -> arithSym('*),
    DIVIDE -> arithSym('/),
    PLUS -> arithSym('+),
    MINUS -> arithSym('-),
    AND -> logicalSym('&&),
    OR -> logicalSym('||),
    LT -> cmpSym('<),
    LE -> cmpSym('<=),
    GT -> cmpSym('>),
    GE -> cmpSym('>=),
    EQ -> miniEqSym,
    NE -> miniNeSym
  )

  def create(lineNum: Int, opStr: String, left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression(lineNum, OP_MAP(opStr), left, right)
  }
}

case class BinaryExpression(lineNum: Int, operator: BinaryOperator, left: Expression, right: Expression) extends Expression {
  def typeCheck(tables: Tables): Type = {
    val leftType = left.typeCheck(tables)
    val rightType = right.typeCheck(tables)

    operator match {
      case TIMES | DIVIDE | PLUS | MINUS | LT | LE | GT | GE =>
        if ((leftType matches IntType()) && (rightType matches IntType())) {
          operator match {
            case TIMES | DIVIDE | PLUS | MINUS => IntType()
            case _ => BoolType()
          }
        } else {
          throw SemanticCheckException(lineNum, s"Operator $operator expects int operands, got $leftType and $rightType")
        }

      case EQ | NE =>
        if ((leftType matches rightType) && !(leftType matches BoolType()))
          BoolType()
        else
          throw SemanticCheckException(lineNum, s"Invalid operands for $operator")

      case AND | OR =>
        if ((leftType matches BoolType()) && (rightType matches BoolType()))
          BoolType()
        else
          throw SemanticCheckException(lineNum, s"Operator $operator expects boolean operands, got $leftType and $rightType")
    }
  }

  def interp(state: State): MiniValues = {
    val MiniValues(leftVal, leftSymVal) = Expression.interp(left, state)
    val MiniValues(rightVal, rightSymVal) = Expression.interp(right, state)
    if (operator == DIVIDE) {
      if (rightVal == Num(0)) {
        state.constraints += CmpS('==, rightSymVal.asInstanceOf[SymbolicNum], NumS(0))
        throw MiniInterpreterException(lineNum, "Division by zero")
      } else {
        state.constraints += CmpS('!=, rightSymVal.asInstanceOf[SymbolicNum], NumS(0))
      }
    }
    val concVal = INTERP_MAP(operator)(leftVal, rightVal)
    val symVal = INTERP_SYM_MAP(operator)(leftSymVal, rightSymVal)
    val optSimVal = symVal match {
      case ArithS(op, NumS(l), NumS(r)) => op match {
        case '+ => NumS(l + r)
        case '- => NumS(l - r)
        case _ => symVal
      }
      case LogicS(op, BoolS(left), BoolS(right)) => op match {
        case '&& => BoolS(left && right)
        case '|| => BoolS(left || right)
        case _ => symVal
      }
      case _ => symVal
    }
    MiniValues(concVal, optSimVal)
  }

  def gatherTerms: Set[MiniAST] = Set(left, right).flatMap(_.gatherTerms) + this
}