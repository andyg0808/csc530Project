package work.k33.calpoly.csc530

case class Result[T](
    result: Either[String, String],
    constraints: List[SymbolicBool],
    numSymbols: Int,
    coverage: Set[T],
    inputs: List[Int]
)
