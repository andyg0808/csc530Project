package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.expression._
import work.k33.calpoly.csc530.mini.ast.lvalue.{LValueDot, LValueId}
import work.k33.calpoly.csc530.mini.ast.statement._
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

import scala.collection.JavaConverters._

class MiniToAstStatementVisitor extends MiniBaseVisitor[Statement] {
  final private val expressionVisitor = new MiniToAstExpressionVisitor

  override def visitNestedBlock(ctx: MiniParser.NestedBlockContext): Statement = visit(ctx.block)

  override def visitAssignment(ctx: MiniParser.AssignmentContext): Statement = {
    var expression: Expression = null
    if (ctx.expression != null) expression = expressionVisitor.visit(ctx.expression)
    else expression = ReadExpression(ctx.getStart.getLine)
    AssignmentStatement(ctx.getStart.getLine, visitLvalue(ctx.lvalue), expression)
  }

  override def visitPrint(ctx: MiniParser.PrintContext) = PrintStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression))

  override def visitPrintLn(ctx: MiniParser.PrintLnContext) = PrintLnStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression))

  override def visitConditional(ctx: MiniParser.ConditionalContext) = ConditionalStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression), visit(ctx.thenBlock), if (ctx.elseBlock != null) visit(ctx.elseBlock)
  else BlockStatement.emptyBlock)

  override def visitWhile(ctx: MiniParser.WhileContext) = WhileStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression), visit(ctx.block))

  override def visitDelete(ctx: MiniParser.DeleteContext) = DeleteStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression))

  override def visitReturn(ctx: MiniParser.ReturnContext): Statement = if (ctx.expression != null) ReturnStatement(ctx.getStart.getLine, expressionVisitor.visit(ctx.expression))
  else ReturnEmptyStatement(ctx.getStart.getLine)

  override def visitInvocation(ctx: MiniParser.InvocationContext) = InvocationStatement(ctx.getStart.getLine, InvocationExpression(ctx.getStart.getLine, ctx.ID.getText, gatherArguments(ctx.arguments)))

  private def gatherArguments(ctx: MiniParser.ArgumentsContext) = {
    ctx.expression.asScala.map(expressionVisitor.visit).toList
  }

  override def visitStatementList(ctx: MiniParser.StatementListContext): Statement = {
    BlockStatement(ctx.getStart.getLine, ctx.statement.asScala.map(visit).toList)
  }

  private def visitLvalue(ctx: MiniParser.LvalueContext) = ctx match {
    case lctx: MiniParser.LvalueIdContext =>
      LValueId(lctx.getStart.getLine, lctx.ID.getText)
    case _ =>
      val lctx = ctx.asInstanceOf[MiniParser.LvalueDotContext]
      LValueDot(lctx.getStart.getLine, visitLvalueNested(lctx.lvalue), lctx.ID.getText)
  }

  private def visitLvalueNested(ctx: MiniParser.LvalueContext): Expression = ctx match {
    case lctx: MiniParser.LvalueIdContext =>
      IdentifierExpression(lctx.getStart.getLine, lctx.ID.getText)
    case _ =>
      val lctx = ctx.asInstanceOf[MiniParser.LvalueDotContext]
      DotExpression(lctx.getStart.getLine, visitLvalueNested(lctx.lvalue), lctx.ID.getText)
  }

  override def visitBlock(ctx: MiniParser.BlockContext): Statement = visit(ctx.statementList)

  override protected def defaultResult: Statement = BlockStatement.emptyBlock
}