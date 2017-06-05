package work.k33.calpoly.csc530

import java.nio.file.{Files, Paths}

import work.k33.calpoly.csc530.mini.MiniInterpreter
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.pact._

import scala.Console.{GREEN, RED, RESET}
import scala.collection.mutable
import scala.compat.Platform
import scala.util.Random

/**
 * Simple fuzzer that uses randomized inputs
 */
object Fuzzer extends App {
  def timer(exec: => Unit): Unit = {
    val startTime = System.nanoTime()
    exec
    println(Console.MAGENTA + s"Time taken: ${(System.nanoTime() - startTime)*Math.pow(10, -9)}\n" + Console.RESET)
  }
  override def main(args: Array[String]): Unit = {
    args match {
      case Array(filename) =>
        timer(testFile(filename, None, None, Platform.currentTime))
      case Array(filename, iterationsStr) =>
        timer(testFile(filename, Some(iterationsStr.toInt), None, Platform.currentTime))
      case Array(filename, iterationsStr, timeStr) =>
        timer(testFile(filename, Some(iterationsStr.toInt), Some(timeStr.toDouble), Platform.currentTime))
      case _ =>
        println("usage: scala Fuzzer <filename> [max-iterations] [seed]")
    }
  }

  private def testFile(filename: String, maxIterations: Option[Int], maxTime: Option[Double],
    seed : Long): Unit = {
    if (filename.endsWith(".pact"))
      new Fuzzer(PactInterpreter).test(fileToString(filename), maxIterations, maxTime,
        seed)
    else if (filename.endsWith(".mini"))
      new Fuzzer(MiniInterpreter).test(fileToString(filename), maxIterations, maxTime,
        seed)
    else
      println("unsupported file type: file must end with .pact or .mini")
  }

  private def fileToString(filename: String): String = {
    new String(Files.readAllBytes(Paths.get(filename)))
  }
}

class Fuzzer[T](interpreter: Interpreter[T]) {
  /**
    *
    * @param ast           The AST of the program to test.
    * @param maxIterations The maximum number of iterations to run
    * @param f             A function to run on each intermediate result. Can be used to print output if desired.
    * @return number of times the program was run
    */
  def test(ast: T, maxIterations: Option[Int], maxTime: Option[Double], f: Result[T] => Unit,
    seed : Long): Int = {
    val seeder = new Random(seed)
    var iterations = 0
    val endTime = maxTime.map(_ * 1000000000 + System.nanoTime())
    while (maxIterations.forall(iterations < _) && endTime.forall(System.nanoTime() < _)) {
      val res = interpreter.execute(ast, new RandomInputProvider(seeder.nextLong()))
      f(res)
      val Result(_, fullConstraints, numSymbols, _, _) = res
      iterations += 1
    }
    iterations
  }

  def test(program: String, maxIterations: Option[Int], endTime: Option[Double], seed : Long): Unit = {
    val ast = interpreter.parse(program)
    val maxCoverage = interpreter.gatherTerms(ast)
    val totalCoverage = mutable.Set[T]()

    def retFunc(r: Result[T]) = {
      val Result(result, _, _, coverage, inputs) = r
      totalCoverage ++= coverage
      val inputStr = inputs.mkString("[", ", ", "]")
      result match {
        case Left(errorMsg) => println(s"Inputs $inputStr:\n    ${Console.RED}$errorMsg${Console.RESET}")
        case Right(res) => println(s"Inputs $inputStr:\n    ${Console.GREEN}Result: $res ${Console.RESET}")
      }
    }

    val iterations = test(ast, maxIterations, endTime, retFunc, seed)
    println(s"\nCoverage: ${totalCoverage.size}/${maxCoverage.size}")
    println(s"Iterations: $iterations\n")

    val coverableLines = maxCoverage.flatMap {
      case miniAst: MiniAST => Some(miniAst.lineNum)
      case _ => None
    }

    val nonCoveredLines = (maxCoverage -- totalCoverage).flatMap {
      case miniAst: MiniAST => Some(miniAst.lineNum)
      case _ => None
    }

    program.split("\n").zipWithIndex.foreach {
      case (line, idx) =>
        if (!coverableLines.contains(idx + 1)) {
          println(line)
        } else if (nonCoveredLines.contains(idx + 1)) {
          println(RED + line + RESET)
        } else {
          println(GREEN + line + RESET)
        }
    }
  }
}
