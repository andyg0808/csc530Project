package work.k33.calpoly.csc530.pact

import scala.io.StdIn
import scala.util.Random

trait InputProvider {
  def readInt(): Int
}

object StdInProvider extends InputProvider {
  def readInt(): Int = StdIn.readInt()
}

class ConcolicInputProvider(inputs: Map[Int, Int]) extends InputProvider {
  private var curIndex = 0
  def readInt(): Int = {
    val value = inputs.getOrElse(curIndex, Random.nextInt() % 1000 - 500)
    curIndex += 1
    value
  }
}