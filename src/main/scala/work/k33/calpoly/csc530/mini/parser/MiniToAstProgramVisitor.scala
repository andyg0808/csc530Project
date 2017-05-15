package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.Program
import work.k33.calpoly.csc530.mini.ast.types.Declaration
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

import scala.collection.JavaConverters._

class MiniToAstProgramVisitor extends MiniBaseVisitor[Program] {
  final private val typeDeclarationVisitor = new MiniToAstTypeDeclarationVisitor
  final private val declarationsVisitor = new MiniToAstDeclarationsVisitor
  final private val functionVisitor = new MiniToAstFunctionVisitor

  override def visitProgram(ctx: MiniParser.ProgramContext) = Program(gatherTypes(ctx.types), gatherDeclarations(ctx.declarations), gatherFunctions(ctx.functions))

  private def gatherTypes(ctx: MiniParser.TypesContext) = {
    ctx.typeDeclaration.asScala.map(typeDeclarationVisitor.visit).toList
  }

  private def gatherDeclarations(ctx: MiniParser.DeclarationsContext): List[Declaration] = {
    declarationsVisitor.visit(ctx)
  }

  private def gatherFunctions(ctx: MiniParser.FunctionsContext) = {
    ctx.function.asScala.map(functionVisitor.visit).toList
  }

  override protected def defaultResult = Program(List(), List(), List())
}