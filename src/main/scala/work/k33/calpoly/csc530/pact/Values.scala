package work.k33.calpoly.csc530.pact

/** Stores a concrete and associated symbolic value */
case class Values(
    concrete: Value,
    symbolic: SymbolicValue
)