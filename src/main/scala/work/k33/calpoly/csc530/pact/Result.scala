package work.k33.calpoly.csc530.pact

case class Result(
    result: Either[String, String],
    constraints: List[SymbolicBool],
    numSymbols: Int,
    coverage: Set[ExprC],
    inputs: List[Int]
)
