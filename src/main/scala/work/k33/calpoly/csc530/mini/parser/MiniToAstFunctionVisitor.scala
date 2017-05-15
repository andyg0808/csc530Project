package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.Function
import work.k33.calpoly.csc530.mini.ast.statement.BlockStatement
import work.k33.calpoly.csc530.mini.ast.types.{Declaration, VoidType}
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

import scala.collection.JavaConverters._

class MiniToAstFunctionVisitor extends MiniBaseVisitor[Function] {
  final private val typeVisitor = new MiniToAstTypeVisitor
  final private val declarationsVisitor = new MiniToAstDeclarationsVisitor
  final private val statementVisitor = new MiniToAstStatementVisitor

  override def visitFunction(ctx: MiniParser.FunctionContext): Function = {
    Function(ctx.getStart.getLine,
      ctx.ID.getText,
      gatherParameters(ctx.parameters),
      typeVisitor.visit(ctx.returnType),
      declarationsVisitor.visit(ctx.declarations),
      statementVisitor.visit(ctx.statementList).asInstanceOf[BlockStatement])
  }

  private def gatherParameters(ctx: MiniParser.ParametersContext) = {
    ctx.decl.asScala.toList.map(dctx => Declaration(dctx.getStart.getLine, typeVisitor.visit(dctx.`type`), dctx.ID.getText))
  }

  override protected def defaultResult = Function(-1, "invalid", List(), new VoidType, List(), BlockStatement.emptyBlock)
}