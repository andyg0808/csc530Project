package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.expression._
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}
import scala.collection.JavaConverters._

class MiniToAstExpressionVisitor extends MiniBaseVisitor[Expression] {
  override def visitIntegerExpr(ctx: MiniParser.IntegerExprContext) = IntegerExpression(ctx.getStart.getLine, ctx.INTEGER.getText)

  override def visitTrueExpr(ctx: MiniParser.TrueExprContext) = TrueExpression(ctx.getStart.getLine)

  override def visitIdentifierExpr(ctx: MiniParser.IdentifierExprContext) = IdentifierExpression(ctx.getStart.getLine, ctx.ID.getText)

  override def visitBinaryExpr(ctx: MiniParser.BinaryExprContext): Expression = BinaryExpression.create(ctx.op.getLine, ctx.op.getText, visit(ctx.lft), visit(ctx.rht))

  override def visitNewExpr(ctx: MiniParser.NewExprContext) = NewExpression(ctx.getStart.getLine, ctx.ID.getText)

  override def visitDotExpr(ctx: MiniParser.DotExprContext) = DotExpression(ctx.getStart.getLine, visit(ctx.expression), ctx.ID.getText)

  override def visitUnaryExpr(ctx: MiniParser.UnaryExprContext): Expression = UnaryExpression.create(ctx.op.getLine, ctx.op.getText, visit(ctx.expression))

  override def visitInvocationExpr(ctx: MiniParser.InvocationExprContext) = InvocationExpression(ctx.getStart.getLine, ctx.ID.getText, gatherArguments(ctx.arguments))

  override def visitFalseExpr(ctx: MiniParser.FalseExprContext) = FalseExpression(ctx.getStart.getLine)

  override def visitNullExpr(ctx: MiniParser.NullExprContext) = NullExpression(ctx.getStart.getLine)

  private def gatherArguments(ctx: MiniParser.ArgumentsContext) = {
    ctx.expression.asScala.map(visit).toList
  }

  override def visitNestedExpr(ctx: MiniParser.NestedExprContext): Expression = visit(ctx.expression)

  override def defaultResult = NullExpression(-1)
}