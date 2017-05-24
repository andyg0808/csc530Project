package work.k33.calpoly.csc530

import java.nio.file.{Files, Paths}

import Console.{GREEN, RED, RESET}
import work.k33.calpoly.csc530.mini.MiniInterpreter
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.mini.ast.statement.Statement
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
  /**
    *
    * @param ast           The AST of the program to test.
    * @param maxIterations The maximum number of iterations to run
    * @param f             A function to run on each intermediate result. Can be used to print output if desired.
    * @return true if all paths were checked
    */
  def test(ast: T, maxIterations: Option[Int], f: Result[T] => Unit): Int = {
    var iterations = 0
    val workList: mutable.Queue[Input] = mutable.Queue(Input(Map(), 1))
    while (workList.nonEmpty && maxIterations.forall(iterations < _)) {
      val input = workList.dequeue()
      val res = interpreter.execute(ast, new ConcolicInputProvider(input.inputs))
      f(res)
      val Result(_, fullConstraints, numSymbols, _, _) = res
      // Remove unneeded constraints
      val constraints = fullConstraints.filter{
        case BoolS(_) => false
        case _ => true
      }
      val start = constraints.takeRight(input.bound-1)
      val solver = new ConstraintSolver(start, numSymbols)
      var lastHead: Option[SymbolicBool] = None
      for (i <- input.bound to constraints.size) {
        val pc = constraints.takeRight(i)
        if (lastHead.isDefined) {
          solver.add(List(lastHead.get))
        }
        lastHead = Some(pc.head)
        solver.push()
        solver.add(List(NotS(pc.head)))
        solver.solve().foreach(
          newInput => workList.enqueue(Input(newInput, i + 1)))
        solver.pop()
      }
      solver.close()
      iterations += 1
    }
    iterations
  }

  def test(program: String, maxIterations: Option[Int]): Unit = {
    val ast = interpreter.parse(program)
    val maxCoverage = interpreter.gatherTerms(ast)
    val totalCoverage = mutable.Set[T]()
    val retFunc = (r: Result[T]) => {
      val Result(result, _, _, coverage, inputs) = r
      totalCoverage ++= coverage
      val inputStr = inputs.mkString("[", ", ", "]")
      result match {
        case Left(errorMsg) => println(s"Inputs $inputStr:\n    Caused ***$errorMsg***")
        case Right(res) => println(s"Inputs $inputStr:\n    Result: $res ")
      }
    }
    val iterations = test(ast, maxIterations, retFunc)
    println(s"\nCoverage: ${totalCoverage.size}/${maxCoverage.size}")
    println(s"Iterations: ${iterations}")
    val coveredLines = new mutable.BitSet()
    totalCoverage.foreach(line => {
      line match {
        case s: Statement => {
          if (s.lineNum > 0) {
            coveredLines += s.lineNum
          }
        }
        case _ =>
      }
    })
    program.split("\n").zipWithIndex.foreach{case (line, idx) => {
      if (coveredLines.contains(idx + 1)) {
        println(GREEN + line + RESET)
      } else {
        println(RED + line + RESET)
      }
    }}
  }
}
