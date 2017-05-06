package work.k33.calpoly.csc530.pact

import scala.collection.immutable.ListMap

case class PactParserException(term: SExp) extends RuntimeException(s"Illegal expression $term")

object PactParser {
  def parse(term: SExp): ExprC = {
    term match {
      case SInt(num) =>
        NumC(num)
      case SSymbol(sym) if validId(sym) =>
        IdC(sym)
      case SList(SSymbol('if), guard, ifTrue, ifFalse) =>
        IfC(parse(guard), parse(ifTrue), parse(ifFalse))
      case SList(SSymbol('lam), params, body) if validParams(params) =>
        LamC(params.toList.map(_.asInstanceOf[SSymbol].value), parse(body))
      case SList(SSymbol('var), assignments, body) if validAssignments(assignments) =>
        val assignmentMap = extractAssignments(assignments)
        AppC(LamC(assignmentMap.keys.toList, parse(body)), assignmentMap.values.toList.map(parse))
      case SCons(func, args) =>
        AppC(parse(func), args.toList.map(parse))
      case _ =>
        throw PactParserException(term)
    }
  }

  private def validId(sym: Symbol): Boolean = {
    !List('var, 'if, 'lam, '=).contains(sym)
  }

  private def validParams(sexp: SExp): Boolean = {
    sexp match {
      case SNil() => true
      case SCons(SSymbol(sym), rest) if validId(sym) =>
        validParams(rest)
      case _ => false
    }
  }

  private def validAssignments(assignments: SExp): Boolean = {
    assignments match {
      case SNil() => true
      case SCons(SList(SSymbol(sym), SSymbol('=), _), rest) if validId(sym) =>
        validAssignments(rest)
      case _ => false
    }
  }

  private def extractAssignments(assignments: SExp): ListMap[Symbol, SExp] = {
    assignments match {
      case SNil() => ListMap()
      case SCons(SList(SSymbol(target), SSymbol('=), source), rest) =>
        extractAssignments(rest) + (target -> source)
    }
  }
}
