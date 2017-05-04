package work.k33.calpoly.csc530

import com.microsoft.z3.{BoolExpr, Context, Expr, IntNum}

object Basic {
  def main(args: Array[String]): Unit = {
    first()
  }

  def first() {
    val ctx = new Context()
    val x = ctx.mkIntConst("x")
    val y = ctx.mkIntConst("y")
    val assert1 = ctx.mkEq(x, y)
    val assert2 = ctx.mkEq(ctx.mkMul(x, ctx.mkInt(2)), ctx.mkAdd(x, ctx.mkInt(10)))
    val combined = ctx.mkAnd(assert1, assert2)
    findExample(ctx, combined, false, Array(x, y))
  }

  /*
  Slightly based on example from Z3 (https://github.com/Z3Prover/z3)
  */
  def findExample(ctx: Context, f: BoolExpr, useMBQI: Boolean, consts: Array[Expr]) {
    val s = ctx.mkSolver()
    s.add(f)
    val q = s.check()
    val model = s.getModel
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



