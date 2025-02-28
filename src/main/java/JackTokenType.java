public enum JackTokenType {
    KEYWORD("keyword"), SYMBOL("symbol"), INT_CONST("integerConstant"),
    STRING_CONST("stringConstant"), IDENTIFIER("identifier");

    private String value;

    JackTokenType(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
