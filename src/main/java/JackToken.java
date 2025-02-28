public class JackToken {
    private JackTokenType type;
    private String lexeme;
    private Object literal;

    public JackToken(JackTokenType type, String lexeme, Object literal) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
    }

    public JackTokenType getType() {
        return type;
    }

    public String getLexeme() {
        return lexeme;
    }

    public Object getLiteral() {
        return literal;
    }
}
