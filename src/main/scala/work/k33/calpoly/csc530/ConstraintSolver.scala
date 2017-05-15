package work.k33.calpoly.csc530

import com.microsoft.z3._

class ConstraintSolver(constraints: List[SymbolicBool], numSymbols: Int) {
  val context = new Context()
  val constants: Map[Int, IntExpr] = (0 until numSymbols).map(idx => idx -> context.mkIntConst(s"x$idx")).toMap

  def solve(): Option[Map[Int, Int]] = {
    val solver = context.mkSolver()
    // I'm not sure that adding multiple constraints is better than anding all the constraints, but
    // I'm guessing it's better to give the solver more information about the constraints.
    constraints.map(toZ3Expr).foreach(solver.add(_))
    val status = solver.check()

    status match {
      case Status.SATISFIABLE =>
        val model = solver.getModel
        Some(constants.flatMap({
          case (idx, const) =>
            model.getConstInterp(const) match {
              case intNum: IntNum => Some(idx -> intNum.getInt64.toInt)
              case null => None
            }
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
          case '&& => context.mkAnd(leftExpr, rightExpr)
          case '|| => context.mkOr(leftExpr, rightExpr)
        }
      case NotS(boolSym) =>
        context.mkNot(toZ3Expr(boolSym))
    }
  }
}
