package work.k33.calpoly.csc530.pact

import work.k33.calpoly.csc530.pact.PactInterpreter._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class PactInterpreterException(msg: String) extends RuntimeException(msg)

object PactInterpreter {
  type Env = Map[Symbol, Value]
  type SymbolicEnv = Map[Symbol, SymbolicValue]
}

class PactInterpreter(inputProvider: InputProvider) {
  var symbolicVariableIndex = 0
  val coverage: mutable.Set[ExprC] = mutable.Set()
  val inputsRead: ArrayBuffer[Int] = ArrayBuffer()
  val constraints: ArrayBuffer[SymbolicBool] = ArrayBuffer()

  def topInterp(program: String): String = {
    execute(PactParser.parse(SExp.from(program))).result.fold(identity, identity)
  }

  def execute(expr: ExprC): Result = {
    val result: Either[String, String] =
      try {
        val Values(value, _) = interp(expr, INITIAL_ENV, INITIAL_SYMBOLIC_ENV)
        Right(serialize(value))
      } catch {
        case PactInterpreterException(msg) => Left(s"Error: $msg")
      }
    Result(result, constraints.reverseIterator.toList, symbolicVariableIndex, coverage.toSet, inputsRead.toList)
  }

  def interp(expr: ExprC, env: Env, symEnv: SymbolicEnv): Values = {
    coverage += expr
    expr match {
      case NumC(num) => Values(NumV(num), NumS(num))
      case IdC(sym) =>
        val value = env.getOrElse(sym, throw PactInterpreterException(s"Unbound id $sym"))
        val symVal = symEnv(sym)
        Values(value, symVal)
      case LamC(params, body) => Values(CloV(params, body, env, symEnv), FuncS())
      case IfC(guard, ifTrue, ifFalse) =>
        interp(guard, env, symEnv) match {
          case Values(BoolV(bool), symbolicCondition: SymbolicBool) =>
            constraints += (if (bool) symbolicCondition else NotS(symbolicCondition))
            val Values(value, symVal) = interp(if (bool) ifTrue else ifFalse, env, symEnv)
            Values(value, symVal)
          case _ => throw PactInterpreterException("Non-boolean guard")
        }
      case AppC(func, args) =>
        interp(func, env, symEnv) match {
          case Values(CloV(params, body, cloEnv, cloSymEnv), _) =>
            val argResults = args.map(interp(_, env, symEnv))
            val (newEnv, newSymEnv) = extendEnv(cloEnv, cloSymEnv, params, argResults)
            interp(body, newEnv, newSymEnv)
          case Values(PrimV(proc), _) =>
            val argResults = args.map(interp(_, env, symEnv))
            proc(argResults)
          case _ => throw PactInterpreterException("Application of non-function")
        }
    }
  }

  def serialize(value: Value): String = {
    value match {
      case NumV(num) => num.toString
      case BoolV(bool) => bool.toString
      case CloV(_, _, _, _) | PrimV(_) => "#<procedure>"
    }
  }

  private final val INITIAL_ENV = Map(
    'true -> BoolV(true),
    'false -> BoolV(false),
    '+ -> PrimV(pact_+),
    '- -> PrimV(pact_-),
    '* -> PrimV(pact_*),
    '/ -> PrimV(pact_/),
    '< -> PrimV(pactCmp(_ < _, '<)),
    '<= -> PrimV(pactCmp(_ <= _, '<=)),
    '> -> PrimV(pactCmp(_ > _, '>)),
    '>= -> PrimV(pactCmp(_ >= _, '>=)),
    '== -> PrimV(pact_==),
    '!= -> PrimV(pact_!=),
    'not -> PrimV(pactNot),
    'input -> PrimV(pactInput))

  private final val INITIAL_SYMBOLIC_ENV = Map(
    'true -> BoolS(true),
    'false -> BoolS(false),
    '+ -> FuncS(),
    '- -> FuncS(),
    '* -> FuncS(),
    '/ -> FuncS(),
    '< -> FuncS(),
    '<= -> FuncS(),
    '> -> FuncS(),
    '>= -> FuncS(),
    '== -> FuncS(),
    '!= -> FuncS(),
    'not -> FuncS(),
    'input -> FuncS())

  private def extendEnv(env: Env, symEnv: SymbolicEnv, params: List[Symbol], args: List[Values]): (Env, SymbolicEnv) = {
    if (params.size == args.size) {
      (env ++ params.zip(args.map(_.concrete)), symEnv ++ params.zip(args.map(_.symbolic)))
    } else {
      throw PactInterpreterException("Arity mismatch")
    }
  }

  private def pact_+(args: List[Values]): Values = {
    if (allNums(args)) {
      val concVal = NumV(args.map(_.concrete.asInstanceOf[NumV].num).sum)
      val symVal = args.map(_.symbolic.asInstanceOf[SymbolicNum])
                   .fold(NumS(0))((res, symVal) => ArithS('+, res, symVal))
      Values(concVal, symVal)
    } else {
      throw PactInterpreterException("Invalid arguments to +")
    }
  }

  private def pact_-(args: List[Values]): Values = {
    args match {
      case List(Values(NumV(num), symVal: SymbolicNum)) =>
        Values(NumV(-num), ArithS('-, NumS(0), symVal))
      case List(Values(NumV(left), leftSymVal: SymbolicNum), Values(NumV(right), rightSymVal: SymbolicNum)) =>
        Values(NumV(left - right), ArithS('-, leftSymVal, rightSymVal))
      case _ => throw PactInterpreterException("Invalid arguments to -")
    }
  }

  private def pact_*(args: List[Values]): Values = {
    if (allNums(args)) {
      val concVal = NumV(args.map(_.concrete.asInstanceOf[NumV].num).product)
      val symVal = args.map(_.symbolic.asInstanceOf[SymbolicNum])
                   .fold(NumS(1))((res, symVal) => ArithS('*, res, symVal))
      Values(concVal, symVal)
    } else {
      throw PactInterpreterException("Invalid arguments to *")
    }
  }

  private def pact_/(args: List[Values]): Values = {
    args match {
      case List(_, Values(NumV(0), zeroSymVal: SymbolicNum)) =>
        constraints += CmpS('==, zeroSymVal, NumS(0))
        throw PactInterpreterException("Division by zero")
      case List(Values(NumV(left), leftSymVal: SymbolicNum), Values(NumV(right), rightSymVal: SymbolicNum)) =>
        constraints += CmpS('!=, rightSymVal, NumS(0))
        Values(NumV(left / right), ArithS('/, leftSymVal, rightSymVal))
      case _ => throw PactInterpreterException("Invalid arguments to /")
    }
  }

  private def pactCmp(cmp: (Int, Int) => Boolean, cmpName: Symbol)(args: List[Values]): Values = {
    args match {
      case List(Values(NumV(left), leftSymVal: SymbolicNum), Values(NumV(right), rightSymVal: SymbolicNum)) =>
        Values(BoolV(cmp(left, right)), CmpS(cmpName, leftSymVal, rightSymVal))
      case _ => throw PactInterpreterException(s"Invalid arguments to $cmpName")
    }
  }

  private def pact_==(args: List[Values]): Values = {
    args match {
      case List(Values(NumV(left), leftSymVal: SymbolicNum), Values(NumV(right), rightSymVal: SymbolicNum)) =>
        Values(BoolV(left == right), CmpS('==, leftSymVal, rightSymVal))
      case List(Values(BoolV(left), leftSymVal: SymbolicBool), Values(BoolV(right), rightSymVal: SymbolicBool)) =>
        Values(BoolV(left == right), LogicS('==, leftSymVal, rightSymVal))
      case List(_, _) => Values(BoolV(false), BoolS(false))
      case _ => throw PactInterpreterException(s"Invalid arguments to ==")
    }
  }

  private def pactNot(args: List[Values]): Values = {
    args match {
      case List(Values(BoolV(bool), symVal: SymbolicBool)) =>
        Values(BoolV(!bool), NotS(symVal))
      case _ => throw PactInterpreterException(s"Invalid arguments to not")
    }
  }

  private def pact_!=(args: List[Values]): Values = {
    pactNot(List(pact_==(args)))
  }

  private def pactInput(args: List[Values]): Values = {
    args match {
      case Nil =>
        val input = inputProvider.readInt()
        inputsRead += input
        val vp = Values(NumV(input), IdS(symbolicVariableIndex))
        symbolicVariableIndex += 1
        vp
      case _ => throw PactInterpreterException("input does not accept arguments")
    }
  }

  private def allNums(args: List[Values]): Boolean = {
    args.forall(arg => arg.concrete.isInstanceOf[NumV] && arg.symbolic.isInstanceOf[SymbolicNum])
  }
}
