package work.k33.calpoly.csc530.mini.parser

import work.k33.calpoly.csc530.mini.ast.types.Declaration
import work.k33.calpoly.csc530.mini.parser.generated.{MiniBaseVisitor, MiniParser}

import scala.collection.JavaConverters._

class MiniToAstDeclarationsVisitor extends MiniBaseVisitor[List[Declaration]] {
  final private val typeVisitor = new MiniToAstTypeVisitor

  override def visitDeclarations(ctx: MiniParser.DeclarationsContext): List[Declaration] = {
    ctx.declaration.asScala.flatMap(addDeclarationsTo).toList
  }

  private def addDeclarationsTo(ctx: MiniParser.DeclarationContext): List[Declaration] = {
    ctx.ID.asScala.map(node => Declaration(node.getSymbol.getLine, typeVisitor.visit(ctx.`type`), node.getText)).toList
  }

  override protected def defaultResult: List[Declaration] = List()
}