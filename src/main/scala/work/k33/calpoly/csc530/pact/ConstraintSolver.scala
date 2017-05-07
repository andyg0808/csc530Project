package work.k33.calpoly.csc530.pact

import com.microsoft.z3._

class ConstraintSolver(constraints: List[SymbolicBool], numSymbols: Int) {
  val context = new Context()
  val constants: Map[Int, IntExpr] = (0 until numSymbols).map(idx => idx -> context.mkIntConst(s"x$idx")).toMap

  def solve(): Option[Map[Int, Int]] = {
    val conjunction = context.mkAnd(constraints.map(toZ3Expr): _*)
    val solver = context.mkSolver()
    solver.add(conjunction)
    val status = solver.check()

    status match {
      case Status.SATISFIABLE =>
        val model = solver.getModel
        Some(constants.mapValues(model.getConstInterp(_) match {
          case intNum: IntNum => intNum.getInt
        }))
      case _ => None
    }
  }

  def toZ3Expr(symbolicNum: SymbolicNum): ArithExpr = {
    symbolicNum match {
      case IdS(index) => constants(index)
      case NumS(num) => context.mkInt(num)
      case ArithS(op, left, right) =>
        val leftExpr = toZ3Expr(left)
        val rightExpr = toZ3Expr(right)
        op match {
          case '+ => context.mkAdd(leftExpr, rightExpr)
          case '- => context.mkSub(leftExpr, rightExpr)
          case '* => context.mkMul(leftExpr, rightExpr)
          case '/ => context.mkDiv(leftExpr, rightExpr)
        }
    }
  }

  def toZ3Expr(constraint: SymbolicBool): BoolExpr = {
    constraint match {
      case BoolS(bool) => context.mkBool(bool)
      case CmpS(op, left, right) =>
        val leftExpr = toZ3Expr(left)
        val rightExpr = toZ3Expr(right)
        op match {
          case '< => context.mkLt(leftExpr, rightExpr)
          case '<= => context.mkLe(leftExpr, rightExpr)
          case '> => context.mkGt(leftExpr, rightExpr)
          case '>= => context.mkGe(leftExpr, rightExpr)
          case '== => context.mkEq(leftExpr, rightExpr)
          case '!= => context.mkNot(context.mkEq(leftExpr, rightExpr))
        }
      case LogicS(op, left, right) =>
        val leftExpr = toZ3Expr(left)
        val rightExpr = toZ3Expr(right)
        op match {
          case '== => context.mkNot(context.mkXor(leftExpr, rightExpr))
          case '!= => context.mkXor(leftExpr, rightExpr)
        }
      case NotS(boolSym) =>
        context.mkNot(toZ3Expr(boolSym))
    }
  }
}
