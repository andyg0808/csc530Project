package work.k33.calpoly.csc530.pact

import org.scalatest.{FlatSpec, Matchers}

object PactParserSpec {
  final val TESTCASE: String = "(if (lam () (lam () ((lam () (var ((h = (lam () (if (((var ((e = ((if (var ((c = " +
    "(var ((b = (if (if foo (lam (foo a b c) a) (lam (d e f) 0)) (b (1 (var ((foo = c)) (r (if -1 (var " +
    "((a = 0)) (var () (if o (if (var () (lam (i) (if m 0 (if (if (if i j (var () h)) k 1) l g)))) d " +
    "((((var () -1) (lam (g h) (var () f))) 1) e)) n))) p) q)) s t) u v w) x))) y)) (d = z)) foo) a b)))" +
    " (f = c) (g = d)) e))) f g))) (i = h) (j = i) (k = j)) k))))) l m)"

  final val BAD_TESTCASES: List[String] =
    List(
      "(lam n o p)",
      "(if lam -1 0)",
      "(if q lam r)",
      "(if 1 -1 =)",
      "()",
      "(if)",
      "(if s)",
      "(if 0 t)",
      "(if x 1 y -1)",
      "(if 0 z foo 1 a)",
      "(if b -1 0 c 1 d)",
      "(= e)",
      "(-1 var)",
      "(lam ((l = o)) p)",
      "(var ((lam = -1)) -1)",
      "(var ((m lam q)) b)",
      "(var ((n = if)) 0)",
      "(var (()) c)",
      "(var ((o)) 1)",
      "(var ((p =)) d)",
      "(var ((s = s 1)) 0)",
      "(var ((t = t -1 u)) f)",
      "(var ((u = 0 v 1 w)) 1)",
      "(var ((f = j)) var)",
      "(var)",
      "(var ((g = -1)))",
      "(var ((j = m)) -1 n)",
      "(var ((k = 0)) 1 o -1)",
      "(var ((l = p)) q 0 r 1)",
      "(= (j) s)",
      "(lam (lam) -1)",
      "(lam (v) if)",
      "(lam)",
      "(lam (w))",
      "(lam (z) 1 x)",
      "(lam (foo) -1 y 0)",
      "(lam (a) z 1 foo -1)",
      "lam")
}

class PactParserSpec extends FlatSpec with Matchers {
  "The parser" should "allow syntactically-valid programs" in {
    PactParser.parse(SExp.from(PactParserSpec.TESTCASE))
  }

  PactParserSpec.BAD_TESTCASES.foreach(badTestcase =>
    it should s"throw an exception on $badTestcase" in {
      intercept[PactParserException] {
        PactParser.parse(SExp.from(badTestcase))
      }
    }
  )
}
