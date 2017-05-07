package work.k33.calpoly.csc530.pact

import java.nio.file.{Files, Paths}

import scala.collection.mutable

case class Input(inputs: Map[Int, Int], bound: Int)

object PactConcolicTester extends App {
  override def main(args: Array[String]): Unit = {
    args match {
      case Array(filename) =>
        test(fileToString(filename), None)
      case Array(filename, iterationsStr) =>
        test(fileToString(filename), Some(iterationsStr.toInt))
      case _ =>
        println("usage: scala PactConcolicTester <filename> [max-iterations]")
    }
  }

  def test(program: String, maxIterations: Option[Int]): Unit = {
    var iterations = 0
    val ast = PactParser.parse(SExp.from(program))
    val maxCoverage = getAllExpressions(ast)
    val totalCoverage = mutable.Set[ExprC]()
    val workList: mutable.Queue[Input] = mutable.Queue(Input(Map(), 1))
    while (workList.nonEmpty && maxIterations.forall(iterations < _)) {
      val input = workList.dequeue()
      val interpreter = new PactInterpreter(new ConcolicInputProvider(input.inputs))
      val Result(result, constraints, numSymbols, coverage, inputs) = interpreter.execute(ast)
      totalCoverage ++= coverage
      val inputStr = inputs.mkString("[", ", ", "]")
      result match {
        case Left(errorMsg) => println(s"Inputs $inputStr:\n    Caused ***$errorMsg***")
        case Right(res) => println(s"Inputs $inputStr:\n    Result: $res ")
      }
      for (i <- input.bound to constraints.size) {
        val pc = constraints.takeRight(i)
        val toSolve = NotS(pc.head) :: pc.tail
        new ConstraintSolver(toSolve, numSymbols).solve().foreach(
          newInput => workList.enqueue(Input(newInput, i + 1)))
      }
      iterations += 1
    }
    println(s"\nCoverage: ${totalCoverage.size}/${maxCoverage.size}")
    println(s"Iterations: $iterations")
  }

  def getAllExpressions(expr: ExprC): Set[ExprC] = {
    val children: Set[ExprC] = expr match {
      case LamC(_, body) => getAllExpressions(body)
      case IfC(i, t, e) => Set(i, t, e).flatMap(getAllExpressions)
      case AppC(func, args) => (Set(func) ++ args).flatMap(getAllExpressions)
      case _ => Set()
    }
    children + expr
  }

  private def fileToString(filename: String): String = {
    new String(Files.readAllBytes(Paths.get(filename)))
  }
}
