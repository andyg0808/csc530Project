package work.k33.calpoly.csc530.mini.ast

trait MiniAST {
  def gatherTerms: Set[MiniAST]
  def lineNum: Int
}
