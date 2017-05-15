package work.k33.calpoly.csc530

import java.nio.file.{Files, Paths}

import work.k33.calpoly.csc530.mini.MiniInterpreter
import work.k33.calpoly.csc530.pact._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Input(inputs: Map[Int, Int], bound: Int)

object ConcolicTester extends App {
  override def main(args: Array[String]): Unit = {
    args match {
      case Array(filename) =>
        testFile(filename, None)
      case Array(filename, iterationsStr) =>
        testFile(filename, Some(iterationsStr.toInt))
      case _ =>
        println("usage: scala ConcolicTester <filename> [max-iterations]")
    }
  }

  private def testFile(filename: String, maxIterations: Option[Int]): Unit = {
    if (filename.endsWith(".pact"))
      new ConcolicTester(PactInterpreter).test(fileToString(filename), maxIterations)
    else if (filename.endsWith(".mini"))
      new ConcolicTester(MiniInterpreter).test(fileToString(filename), maxIterations)
    else
      println("unsupported file type: file must end with .pact or .mini")
  }

  private def fileToString(filename: String): String = {
    new String(Files.readAllBytes(Paths.get(filename)))
  }
}

class ConcolicTester[T](interpreter: Interpreter[T]) {
  def test(ast: T, maxIterations: Option[Int]): Seq[Result[T]] = {
    val ret = new ArrayBuffer[Result[T]]
    var iterations = 0
    val workList: mutable.Queue[Input] = mutable.Queue(Input(Map(), 1))
    while (workList.nonEmpty && maxIterations.forall(iterations < _)) {
      val input = workList.dequeue()
      val res = interpreter.execute(ast, new ConcolicInputProvider(input.inputs))
      ret += res
      val Result(_, constraints, numSymbols, _, _) = res
      for (i <- input.bound to constraints.size) {
        val pc = constraints.takeRight(i)
        val toSolve = NotS(pc.head) :: pc.tail
        new ConstraintSolver(toSolve, numSymbols).solve().foreach(
          newInput => workList.enqueue(Input(newInput, i + 1)))
      }
      iterations += 1
    }
    ret
  }

  def test(program: String, maxIterations: Option[Int]): Unit = {
    val ast = interpreter.parse(program)
    val maxCoverage = interpreter.gatherTerms(ast)
    val totalCoverage = mutable.Set[T]()
    val results = test(ast, maxIterations)
    results.foreach(r => {
      val Result(result, _, _, coverage, inputs) = r
      totalCoverage ++= coverage
      val inputStr = inputs.mkString("[", ", ", "]")
      result match {
        case Left(errorMsg) => println(s"Inputs $inputStr:\n    Caused ***$errorMsg***")
        case Right(res) => println(s"Inputs $inputStr:\n    Result: $res ")
      }
    })
    println(s"\nCoverage: ${totalCoverage.size}/${maxCoverage.size}")
    println(s"Iterations: ${results.size}")
  }
}
