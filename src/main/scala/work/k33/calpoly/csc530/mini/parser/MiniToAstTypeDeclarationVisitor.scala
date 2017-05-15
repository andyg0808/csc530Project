package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.types.{Declaration, StructDefinition}
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

import scala.collection.JavaConverters._

class MiniToAstTypeDeclarationVisitor extends MiniBaseVisitor[StructDefinition] {
  final private val typeVisitor = new MiniToAstTypeVisitor

  override def visitTypeDeclaration(ctx: MiniParser.TypeDeclarationContext): StructDefinition = {
    StructDefinition(ctx.getStart.getLine, ctx.ID.getText, gatherFieldDeclarations(ctx.nestedDecl))
  }

  private def gatherFieldDeclarations(ctx: MiniParser.NestedDeclContext) = {
    ctx.decl.asScala.map(dctx => Declaration(dctx.getStart.getLine, typeVisitor.visit(dctx.`type`), dctx.ID.getText)).toList
  }

  override protected def defaultResult = StructDefinition(-1, "invalid", List())
}