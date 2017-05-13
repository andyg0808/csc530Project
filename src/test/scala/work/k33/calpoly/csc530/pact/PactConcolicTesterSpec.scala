package work.k33.calpoly.csc530.pact

import org.scalatest.FlatSpec
import org.scalatest.Assertions._
import org.scalatest.Matchers._

/**
  * Created by andrew on 5/8/17.
  */
class PactConcolicTesterSpec extends FlatSpec {
  // Basic checks that new operations don't blow up
  "&&" should "work under concolic testing" in {
    val expr = AppC(LamC(List('a, 'b, 'c),
      IfC(AppC(IdC('&&), List('a, 'b, 'c).map(x => AppC(IdC('==), List(IdC(x), NumC(1))))),
        IdC('true),
        IdC('true))),
      List(IdC('input), IdC('input), IdC('input)))
    val results = PactConcolicTester.test(expr, None)
    results.foreach(r => r.result should be('right))
  }

  "||" should "work under concolic testing" in {
    val expr = AppC(LamC(List('a, 'b, 'c),
      IfC(AppC(IdC('||), List('a, 'b, 'c).map(x => AppC(IdC('==), List(IdC(x), NumC(1))))),
        IdC('true),
        IdC('true))),
      List(IdC('input), IdC('input), IdC('input)))
    val results = PactConcolicTester.test(expr, None)
    results.foreach(r => r.result should be('right))
  }
}
