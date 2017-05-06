package work.k33.calpoly.csc530.pact

/*
 SExp.scala

 Author: Matt Might
 Site: http://matt.might.net/

 A small library to demonstrate how to parse S-Expressions by hand.

 This code is intended primarily to demonstrate Scala to new programmers.

 The grammar itself is also simplified:

  <file> ::= <s-exp-list>

  <s-exp> ::= <atom>
           |  '(' <s-exp-list> ')'

  <s-exp-list> ::= <sexp> <s-exp-list>
                |

  <atom> ::= <symbol>
          |  <integer>
          |  #t  |  #f
*/


import scala.collection.immutable.Stream

/* Exceptions. */

/**
  * Thrown when code is not finished.
  */
case class UnfinishedException() extends RuntimeException


/**
  * Thrown when data is not convertible.
  */
case class InconvertibleException() extends RuntimeException


/**
  * Thrown when code reaches a supposedly impossible state -- a bug.
  */
case class ImpossibleException() extends RuntimeException

/**
  * Thrown when the input is not a valid S-Expression.
  */
case class ParseException(reason: String) extends RuntimeException(reason)


/* S-Expression types */
abstract class SExp {

  /**
    * Returns a true S-Exp list as Scala list of S-Exps.
    */
  def toList: List[SExp]

  /**
    * Returns true if this is a true S-Exp list; false otherwise.
    */
  def isList: Boolean
}

/**
  * The companion object for <code>SExp</code>.
  */
object SExp {

  private def streamFromIterator[A](iterator: Iterator[A]): Stream[A] = {
    if (iterator.hasNext) {
      iterator.next() #:: streamFromIterator(iterator)
    } else {
      Stream.empty
    }
  }

  /**
    * Converts a string representation of an S-Expression into an SExp.
    */
  def from(input: String): SExp = {
    val parser = new SExpParser(streamFromIterator(input.toIterator))
    parser.nextSExp()
  }

  def test() {
    val input1 = "(* 3 10)"
    val sexp1 = from(input1)
    println(sexp1)

    val input2 = "(lambda (n)\n #t;comment\n )"
    val sexp2 = from(input2)
    println(sexp2)

    val input3 = "bar;foo"
    val sexp3 = from(input3)
    println(sexp3)
  }

  def main(args: Array[String]) {
    test()
  }

}

/**
  * An integer as an S-Expression.
  */
case class SInt(value: Int) extends SExp {
  override def toString: String = value.toString

  def toList = throw InconvertibleException()

  val isList = false
}

/**
  * A symbol as an S-Expression.
  */
case class SSymbol(value: Symbol) extends SExp {
  override def toString: String = value.toString

  def toList = throw InconvertibleException()

  val isList = false
}

/**
  * True as an S-Expression.
  */
case class STrue() extends SExp {
  override def toString = "#t"

  def toList = throw InconvertibleException()

  val isList = false
}

/**
  * False as an S-Expression.
  */
case class SFalse() extends SExp {
  override def toString = "#f"

  def toList = throw InconvertibleException()

  val isList = false
}

/**
  * A cons pair as an S-Expression.
  */
case class SCons(car: SExp, cdr: SExp) extends SExp {

  override def toString: String = "(" + toList.mkString(" ") + ")"

  def toList: List[SExp] = car :: cdr.toList

  val isList: Boolean = cdr.isList
}

/**
  * Nil as an S-Expression.
  */
case class SNil() extends SExp {
  override def toString = "null"

  def toList: List[Nothing] = List.empty

  val isList = true
}


/**
  * SList is an extractor for lists.
  */
object SList {

  /**
    * Matches if the underlying S-Expression is a true list, and
    * converts it to a Scala list.
    */
  def unapplySeq(sexp: SExp): Option[List[SExp]] = {
    if (sexp.isList) {
      Some(sexp.toList)
    } else {
      None
    }
  }
}

/* S-Expression lexer token types. */
private trait SExpToken

private case object LPAR extends SExpToken

private case object RPAR extends SExpToken

private case object EOS extends SExpToken

private case class INT(value: Int) extends SExpToken

private case class HASH(value: String) extends SExpToken

private case class ID(value: String) extends SExpToken


/* A one-off parser for S-Expressions. */
class SExpParser(private val input: Stream[Char]) {

  // Internally, this parser uses recursive descent.

  private val lex = new SExpLexer(input)

  /**
    * Parses an entire file.
    */
  def nextFile(): List[SExp] =
    lex.peek() match {

      case EOS => List.empty

      case _ =>
        val head = nextSExp()
        val tail = nextFile()
        head :: tail
    }

  /**
    * Parses the next S-Expression.
    */
  def nextSExp(): SExp =
    lex.peek() match {

      case EOS => throw ParseException("expected s-exp; got end of input")

      case LPAR =>
        lex.eatLPAR()
        val sexp = nextSExpList()
        lex.eatRPAR()
        sexp

      case INT(value) => lex.next(); SInt(value)
      case ID(value) => lex.next(); SSymbol(Symbol(value))
      case HASH("t") => lex.next(); STrue()
      case HASH("f") => lex.next(); SFalse()
    }

  /**
    * Parses a list of S-Expressions.
    */
  private def nextSExpList(): SExp =
    lex.peek() match {

      case RPAR => SNil()

      case _ =>
        val head = nextSExp()
        val tail = nextSExpList()
        SCons(head, tail)
    }
}


private class SExpLexer(private var input: Stream[Char]) {

  /**
    * The next tokens available.
    */
  private var nextTokens: List[SExpToken] = List.empty

  /**
    * The tail (in reverse order) of the next tokens available.
    */
  private var nextTokensTail: List[SExpToken] = List.empty


  /**
    * Called when the lexer has seen a full token.
    */
  def emit(token: SExpToken) {
    nextTokensTail = token :: nextTokensTail
  }

  /**
    * The current internal state of the lexer.
    */
  private var state: SExpLexerState = INWHITESPACE


  /* Lexical states. */
  private trait SExpLexerState {
    /**
      * Returns the new state after processing a character.
      */
    def process(c: Char): SExpLexerState

    /**
      * Returns the new state after processing end of file.
      */
    def processEOF(): SExpLexerState
  }

  private case object DONE extends SExpLexerState {
    def processEOF(): SExpLexerState = {
      emit(EOS)
      DONE
    }

    def process(c: Char): SExpLexerState = {
      throw ImpossibleException()
    }
  }

  private case object INCOMMENT extends SExpLexerState {
    def processEOF(): SExpLexerState = {
      emit(EOS)
      DONE
    }

    def process(c: Char): SExpLexerState = {
      if (c == '\n')
        INWHITESPACE
      else
        INCOMMENT
    }
  }

  private case class INID(buf: List[Char]) extends SExpLexerState {

    def processEOF(): SExpLexerState = {
      emit(ID(buf.reverse.mkString))
      DONE
    }

    def process(c: Char): SExpLexerState = {
      if (c.isWhitespace) {
        emit(ID(buf.reverse.mkString))
        return INWHITESPACE
      }

      c match {
        case ';' =>
          emit(ID(buf.reverse.mkString))
          INCOMMENT

        case '(' =>
          emit(ID(buf.reverse.mkString))
          emit(LPAR)
          INWHITESPACE

        case ')' =>
          emit(ID(buf.reverse.mkString))
          emit(RPAR)
          INWHITESPACE

        case _ =>
          INID(c :: buf)
      }
    }
  }

  private case class INHASH(buf: List[Char]) extends SExpLexerState {

    def processEOF(): SExpLexerState = {
      emit(HASH(buf.reverse.mkString))
      DONE
    }

    def process(c: Char): SExpLexerState = {
      if (c.isWhitespace) {
        emit(HASH(buf.reverse.mkString))
        return INWHITESPACE
      }

      c match {
        case ';' =>
          emit(HASH(buf.reverse.mkString))
          INCOMMENT

        case '(' =>
          emit(HASH(buf.reverse.mkString))
          emit(LPAR)
          INWHITESPACE

        case ')' =>
          emit(HASH(buf.reverse.mkString))
          emit(RPAR)
          INWHITESPACE

        case _ =>
          INHASH(c :: buf)
      }
    }
  }

  private case class INNUM(buf: List[Char]) extends SExpLexerState {

    def processEOF(): SExpLexerState = {
      emit(INT(buf.reverse.mkString.toInt))
      DONE
    }

    def process(c: Char): SExpLexerState = {
      if (c.isDigit) {
        return INNUM(c :: buf)
      }

      emit(INT(buf.reverse.mkString.toInt))

      val old = input
      input = c #:: old
      INWHITESPACE
    }
  }

  private case object INWHITESPACE extends SExpLexerState {

    def processEOF(): SExpLexerState = {
      emit(EOS)
      DONE
    }

    def process(c: Char): SExpLexerState = {
      if (c.isWhitespace)
        return INWHITESPACE

      if (c.isDigit) {
        return INNUM(List(c))
      }

      c match {
        case ';' => INCOMMENT
        case '#' => INHASH(List())
        case '(' =>
          emit(LPAR)
          INWHITESPACE
        case ')' =>
          emit(RPAR)
          INWHITESPACE

        case _ => INID(List(c))
      }
    }
  }


  /**
    * Processes characters until the lexer emits tokens.
    */
  private def loadTokens() {
    if (nextTokens.nonEmpty)
      return

    if (nextTokensTail.nonEmpty) {
      nextTokens = nextTokensTail.reverse
      nextTokensTail = List.empty
      return
    }

    if (input.isEmpty) {
      state = state.processEOF()
      // This had better load a token:
      nextTokens = nextTokensTail.reverse
      nextTokensTail = List.empty
      return
    }

    while (nextTokensTail.isEmpty && input.nonEmpty) {
      val c = input.head
      input = input.tail
      state = state.process(c)
    }

    if (input.isEmpty)
      state = state.processEOF()

    nextTokens = nextTokensTail.reverse
    nextTokensTail = List.empty
  }


  /**
    * Returns the next available token without consuming it.
    */
  def peek(): SExpToken = {
    loadTokens()
    nextTokens.head
  }

  /**
    * Pulls the next token from the input and returns it.
    */
  def next(): SExpToken = {
    loadTokens()
    val t = nextTokens.head
    nextTokens = nextTokens.tail
    t
  }

  /**
    * Pulls the next token from the input, failing if it's not '('.
    */
  def eatLPAR(): Unit =
    next() match {
      case LPAR =>
      case t => throw ParseException("expected: '('; got: " + t)
    }

  /**
    * Pulls the next token from the input, failing if it's not ')'.
    */
  def eatRPAR(): Unit =
    next() match {
      case RPAR =>
      case t => throw ParseException("expected: ')'; got: " + t)
    }
}
