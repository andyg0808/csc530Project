package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.types._
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

class MiniToAstTypeVisitor extends MiniBaseVisitor[Type] {
  override def visitIntType(ctx: MiniParser.IntTypeContext) = IntType()

  override def visitBoolType(ctx: MiniParser.BoolTypeContext) = BoolType()

  override def visitStructType(ctx: MiniParser.StructTypeContext) = StructType(ctx.getStart.getLine, ctx.ID.getText)

  override def visitReturnTypeReal(ctx: MiniParser.ReturnTypeRealContext): Type = visit(ctx.`type`)

  override def visitReturnTypeVoid(ctx: MiniParser.ReturnTypeVoidContext) = VoidType()

  override protected def defaultResult = VoidType()
}