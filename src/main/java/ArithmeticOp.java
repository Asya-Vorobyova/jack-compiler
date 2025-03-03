public enum ArithmeticOp {
    add("+"), sub("-"), and("&"), or("|"), lt("<"), eq("="),
    gt(">"), neg("-"), not("~");
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

    public static ArithmeticOp fromSymbol(String symbol) {
        for (ArithmeticOp op : values()) {
            if (op.symbol.equals(symbol)) {
                return op;
            }
        }
        return null;
    }

    public String getSymbol() {
        return symbol;
    }
}
