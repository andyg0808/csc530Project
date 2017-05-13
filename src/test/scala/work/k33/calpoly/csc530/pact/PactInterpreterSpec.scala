package work.k33.calpoly.csc530.pact

import org.scalatest.{FlatSpec, Matchers}

object PactInterpreterSpec {
  final val TESTCASE: String = "((lam (empty) ((lam (cons) ((lam (empty?) ((lam (first) ((lam (rest) ((lam (Y)" +
    "((lam (length) ((lam (addup) (addup (cons 3 (cons 17 empty)))) (Y (lam (addup) (lam (l) (if (empty? l) 0" +
    "(+ (first l) (addup (rest l))))))))) (Y (lam (length) (lam (l) (if (empty? l) 0 (+ 1 (length (rest l)))))))))" +
    "((lam (x) (lam (y) (y (lam (z) (((x x) y) z))))) (lam (x) (lam (y) (y (lam (z) (((x x) y) z)))))))) (lam (l)" +
    "(l false)))) (lam (l) (l true)))) (lam (l) (== l empty)))) (lam (a b) (lam (select) (if select a b))))) 13)"
}

class PactInterpreterSpec extends FlatSpec with Matchers {
  "The interpreter" should "evaluate this program to 20" in {
    new PactInterpreter(StdInProvider).topInterp(PactInterpreterSpec.TESTCASE) shouldEqual "20"
  }

  "The && function" should "only be true if all arguments are true" in {
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('&&), List(IdC('true), IdC('true)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('&&), List(IdC('false), IdC('true)))) shouldEqual BoolV(false)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('&&), List(IdC('true), IdC('true), IdC('true)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('&&), List(IdC('true), IdC('true), IdC('false)))) shouldEqual BoolV(false)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('&&), List())) shouldEqual BoolV(true)
  }

  "The || function" should "be true if any argument is true" in {
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List(IdC('true), IdC('true)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List(IdC('false), IdC('true)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List(IdC('true), IdC('true), IdC('true)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List(IdC('true), IdC('true), IdC('false)))) shouldEqual BoolV(true)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List(IdC('false), IdC('false), IdC('false)))) shouldEqual BoolV(false)
    new PactInterpreter(StdInProvider).runAST(AppC(IdC('||), List())) shouldEqual BoolV(false)
  }
}
