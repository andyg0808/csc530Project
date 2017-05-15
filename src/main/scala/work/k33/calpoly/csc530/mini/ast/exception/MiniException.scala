package work.k33.calpoly.csc530.mini.ast.exception

case class SemanticCheckException(lineNum: Int, message: String) extends RuntimeException(message) {
  override def toString: String = s"line $lineNum: $message"
}

case class MiniInterpreterException(lineNum: Int, message: String) extends RuntimeException(message) {
  override def toString: String = s"line $lineNum: $message"
}
