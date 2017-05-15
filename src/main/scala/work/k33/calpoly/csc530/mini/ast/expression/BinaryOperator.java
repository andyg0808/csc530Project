package work.k33.calpoly.csc530.mini.ast.expression;

public enum BinaryOperator {
    TIMES("*"),
    DIVIDE("/"),
    PLUS("+"),
    MINUS("-"),
    LT("<"),
    GT(">"),
    LE("<="),
    GE(">="),
    EQ("=="),
    NE("!="),
    AND("&&"),
    OR("||"),
    XOR("xor");

    String opStr;

    BinaryOperator(String opStr) {
        this.opStr = opStr;
    }

    @Override
    public String toString() {
        return opStr;
    }
}
