package work.k33.calpoly.csc530

import java.io.{BufferedReader, FileWriter}
import java.nio.file.{Files, Paths}

import work.k33.calpoly.csc530.mini.MiniInterpreter
import work.k33.calpoly.csc530.mini.ast.MiniAST
import work.k33.calpoly.csc530.pact._

import scala.Console.{GREEN, RED, RESET}
import scala.collection.mutable

case class Input(inputs: Map[Int, Int], bound: Int)

object ConcolicTester extends App {
  def timer(exec: => Unit): Unit = {
    val startTime = System.nanoTime()
    exec
    println(Console.MAGENTA + s"Time taken: ${(System.nanoTime() - startTime)*Math.pow(10, -9)}\n" + Console.RESET)
  }
  override def main(args: Array[String]): Unit = {
    args match {
      case Array(filename) => timer(testFile(filename, None))

      case Array(filename, iterationsStr) =>
        timer(testFile(filename, Some(iterationsStr.toInt)))
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
    * @return number of times the program was run
    */
  def test(ast: T, maxIterations: Option[Int], f: Result[T] => Unit): Int = {
    var iterations = 0
    val workList: mutable.Queue[Input] = mutable.Queue(Input(Map(), 1))
    var index = 0
    while (workList.nonEmpty && maxIterations.forall(iterations < _)) {
      val input = workList.dequeue()
      val res = interpreter.execute(ast, new ConcolicInputProvider(input.inputs))
      f(res)
      val Result(_, fullConstraints, numSymbols, _, _) = res
      // Remove unneeded constraints
      val constraints = fullConstraints.filter {
        case BoolS(_) => false
        case _ => true
      }

      if (constraints.length > index) {
        val writer = new FileWriter(s"../output/iteration$iterations.dot")
        writer.write("digraph demo {\n")
        var last: Option[SymbolicBool] = None
        var i: Int = 0
        for (const <- constraints) {
          if (last.isDefined) {
            writer.write(s"n${i-1} -> n$i\n")
            writer.write(s"n${i-1} -> r$i\n")
          }
          last = Some(const)
          writer.write(s"""n$i [label="$const"]\n""")
          i += 1
        }
        writer.write("}\n")
        writer.close()
        index = constraints.length
      }
      val start = constraints.takeRight(input.bound - 1)
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

    def retFunc(r: Result[T]) = {
      val Result(result, _, _, coverage, inputs) = r
      totalCoverage ++= coverage
      val inputStr = inputs.mkString("[", ", ", "]")
      result match {
        case Left(errorMsg) => println(s"Inputs $inputStr:\n    ${Console.RED}$errorMsg${Console.RESET}")
        case Right(res) => println(s"Inputs $inputStr:\n    ${Console.GREEN}Result: $res ${Console.RESET}")
      }
    }

    val iterations = test(ast, maxIterations, retFunc)
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
