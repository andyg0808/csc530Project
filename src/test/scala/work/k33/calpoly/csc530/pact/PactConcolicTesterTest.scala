package work.k33.calpoly.csc530.pact

import org.scalatest.FunSuite

/**
  * Created by andrew on 5/8/17.
  */
class PactConcolicTesterTest extends FunSuite {
  test("&& works under concolic testing") {
    val expr = AppC(IdC('&&), List(IdC('a), IdC('b), IdC('c)))

  }
}
