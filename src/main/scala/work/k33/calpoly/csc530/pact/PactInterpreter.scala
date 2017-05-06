package work.k33.calpoly.csc530.pact

import scala.io.StdIn

case class PactInterpreterException(msg: String) extends RuntimeException(msg)

object PactInterpreter {
  type Env = Map[Symbol, Value]

  def topInterp(program: String): String = {
    serialize(interp(PactParser.parse(SExp.from(program)), INITIAL_ENV))
  }

  def interp(expr: ExprC, env: Env): Value = {
    expr match {
      case NumC(num) => NumV(num)
      case IdC(sym) => env.getOrElse(sym, throw PactInterpreterException(s"unbound id $sym"))
      case LamC(params, body) => CloV(params, body, env)
      case IfC(guard, ifTrue, ifFalse) =>
        interp(guard, env) match {
          case BoolV(bool) => interp(if (bool) ifTrue else ifFalse, env)
          case _ => throw PactInterpreterException("Non-boolean guard")
        }
      case AppC(func, args) =>
        interp(func, env) match {
          case CloV(params, body, cloEnv) =>
            val newEnv = extendEnv(cloEnv, params, args.map(interp(_, env)))
            interp(body, newEnv)
          case PrimV(proc) => proc(args.map(interp(_, env)))
          case _ => throw PactInterpreterException("Application of non-function")
        }
    }
  }

  def serialize(value: Value): String = {
    value match {
      case NumV(num) => num.toString
      case BoolV(bool) => bool.toString
      case CloV(_, _, _) | PrimV(_) => "#<procedure>"
    }
  }

  private final val INITIAL_ENV = Map(
    'true -> BoolV(true),
    'false -> BoolV(false),
    '+ -> PrimV(pact_+),
    '- -> PrimV(pact_-),
    '* -> PrimV(pact_*),
    '/ -> PrimV(pact_/),
    '< -> PrimV(pactCmp(_ < _)),
    '<= -> PrimV(pactCmp(_ <= _)),
    '> -> PrimV(pactCmp(_ > _)),
    '>= -> PrimV(pactCmp(_ >= _)),
    '== -> PrimV(pact_==),
    '!= -> PrimV(pact_!=),
    'input -> PrimV(pactInput))

  private def extendEnv(env: Env, params: List[Symbol], args: List[Value]): Env = {
    if (params.size == args.size) {
      env ++ params.zip(args)
    } else {
      throw PactInterpreterException("Arity mismatch")
    }
  }

  private def pact_+(args: List[Value]): NumV = {
    args match {
      case nums: List[NumV] if nums.forall(_.isInstanceOf[NumV]) => NumV(nums.map(_.num).sum)
      case _ => throw PactInterpreterException("Invalid arguments to +")
    }
  }

  private def pact_-(args: List[Value]): NumV = {
    args match {
      case List(NumV(num)) => NumV(-num)
      case List(NumV(left), NumV(right)) => NumV(left - right)
      case _ => throw PactInterpreterException("Invalid arguments to -")
    }
  }

  private def pact_*(args: List[Value]): NumV = {
    args match {
      case nums: List[NumV] if nums.forall(_.isInstanceOf[NumV]) => NumV(nums.map(_.num).product)
      case _ => throw PactInterpreterException("Invalid arguments to *")
    }
  }

  private def pact_/(args: List[Value]): NumV = {
    args match {
      case List(_, NumV(0)) => throw PactInterpreterException("Division by zero")
      case List(NumV(left), NumV(right)) => NumV(left / right)
      case _ => throw PactInterpreterException("Invalid arguments to /")
    }
  }

  private def pactCmp(cmp: (Int, Int) => Boolean)(args: List[Value]): BoolV = {
    args match {
      case List(NumV(left), NumV(right)) => BoolV(cmp(left, right))
      case _ => throw PactInterpreterException("Invalid arguments to comparison")
    }
  }

  private def pact_==(args: List[Value]): BoolV = {
    args match {
      case List(NumV(left), NumV(right)) => BoolV(left == right)
      case List(BoolV(left), BoolV(right)) => BoolV(left == right)
      case _ => BoolV(false)
    }
  }

  private def pact_!=(args: List[Value]): Value = {
    BoolV(!pact_==(args).bool)
  }

  private def pactInput(args: List[Value]): Value = {
    args match {
      case Nil => NumV(StdIn.readInt())
      case _ => throw PactInterpreterException("input does not accept arguments")
    }
  }
}
