package work.k33.calpoly.csc530.mini

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import work.k33.calpoly.csc530.{InputProvider, Interpreter, Result}
import work.k33.calpoly.csc530.mini.ast.{MiniAST, Program}
import work.k33.calpoly.csc530.mini.parser.MiniToAstProgramVisitor
import work.k33.calpoly.csc530.mini.parser.generated.{MiniLexer, MiniParser}

object MiniInterpreter extends Interpreter[MiniAST] {
  def execute(program: MiniAST, inputProvider: InputProvider): Result[MiniAST] = {
    program match {
      case prog: Program => prog.interp(inputProvider)
    }
  }

  def parse(programStr: String): MiniAST = {
    val parseTree = new MiniParser(new CommonTokenStream(new MiniLexer(new ANTLRInputStream(programStr)))).program
    val program = new MiniToAstProgramVisitor().visit(parseTree)
    program.semanticCheck()
    program
  }

  def gatherTerms(ast: MiniAST): Set[MiniAST] = ast.gatherTerms
}
