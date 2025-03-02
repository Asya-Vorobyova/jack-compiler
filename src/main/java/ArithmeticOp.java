public enum ArithmeticOp {
    add("+"), sub("-"), and("&"), or("|"), lt(null), eq(null),
    gt(null), neg("-"), not("!");
    private final String symbol;

    ArithmeticOp(String symbol) {
        this.symbol = symbol;
    }

    public static ArithmeticOp parse(String op) {
        try {
            return valueOf(op);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    public String getSymbol() {
        return symbol;
    }
}
