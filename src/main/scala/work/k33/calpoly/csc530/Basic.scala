package work.k33.calpoly.csc530

import com.microsoft.z3.Status.{SATISFIABLE, UNSATISFIABLE, UNKNOWN}
import com.microsoft.z3._

object Basic {

  def main(args: Array[String]): Unit = {
    first()
    second()
  }

  def first() {
    println("First query:")
    val ctx = new Context()
    val x = ctx.mkIntConst("x")
    val y = ctx.mkIntConst("y")
    val assert1 = ctx.mkEq(x, y)
    val assert2 = ctx.mkEq(ctx.mkMul(x, ctx.mkInt(2)), ctx.mkAdd(x, ctx.mkInt(10)))
    val combined = ctx.mkAnd(assert1, assert2)
    println(combined)
    findExample(ctx, combined, Array(x, y))
  }

  def second(): Unit = {
    println("Second query:")
    val ctx = new Context()
    val m_x = ctx.mkIntConst("m_x")
    val m_y = ctx.mkIntConst("m_x")
    val assert1 = ctx.mkEq(m_x, m_y)
    val assert2 = ctx.mkEq(m_y, ctx.mkAdd(m_x, ctx.mkInt(10)))
    val combined = ctx.mkAnd(assert1, assert2)
    println(combined)
    findExample(ctx, combined, Array(m_x, m_y))
  }

  /*
  Slightly based on example from Z3 (https://github.com/Z3Prover/z3)
  */
  def findExample(ctx: Context, f: BoolExpr, consts: Array[Expr]) {
    val s = ctx.mkSolver()
    s.add(f)
    val q = s.check()
    q match {
      case UNSATISFIABLE => println("No solution exists")
      case SATISFIABLE => printModel(consts, s.getModel)
      case UNKNOWN => println("Could not solve")
    }
  }

  def printModel(consts: Array[Expr], model: Model): Unit = {
    for (i <- consts) {
      val interp = model.getConstInterp(i)
      if (interp.isIntNum()) {
        interp match {
          case intnum: IntNum => println(i + ": " + intnum.getInt)
        }
      }
    }
  }
}



