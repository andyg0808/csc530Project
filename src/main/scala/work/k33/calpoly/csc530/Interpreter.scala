package work.k33.calpoly.csc530

trait Interpreter[T] {
  def parse(program: String): T
  def execute(ast: T, inputProvider: InputProvider): Result[T]
  def gatherTerms(ast: T): Set[T]
}
