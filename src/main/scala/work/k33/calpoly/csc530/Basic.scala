package work.k33.calpoly.csc530
import java.util.HashMap
import com.microsoft.z3.Context
import com.microsoft.z3.BoolExpr

object Basic {
/*
Based on MIT-licensed example from Z3 (https://github.com/Z3Prover/z3)

Z3 License
Copyright (c) Microsoft Corporation
All rights reserved. 
MIT License

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
  def main(args: Array[String]) {
    val cfg = new HashMap[String, String]()
    cfg.put("proof", "true")
    val ctx = new Context(cfg)

    val U = ctx.mkUninterpretedSort(ctx.mkSymbol("U"))
    val g = ctx.mkFuncDecl("g", U, U)

    val x = ctx.mkConst("x", U)
    val y = ctx.mkConst("y", U)

    val gx = g.apply(x)
    val gy = g.apply(y)

    val eq = ctx.mkEq(x, y)
    val f = ctx.mkEq(gx, gy)

    prove(ctx, ctx.mkImplies(eq, f), false)
  }

  def prove(ctx: Context, f: BoolExpr, useMBQI: Boolean) {
   val s = ctx.mkSolver()
   val p = ctx.mkParams()
   p.add("mbqi", useMBQI)
   s.setParameters(p)
   //s.add(ctx.mkNot(f))
   s.add(f)
   val q = s.check()
   println("result: " + q)
   //println("Proof: " + s.getProof())
   println("Model: " + s.getModel())
  }
}



