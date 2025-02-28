import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class JackTokenizer {
    private String source;

    private int current = 0;
    private int start = 0;

    private JackToken currentToken = null;
    private boolean newToken= false;

    private List<JackToken> tokens = new ArrayList<>();

    public JackTokenizer(Path inputFile) throws IOException {
        this.source = Files.readString(inputFile);
    }

    public boolean hasMoreTokens() {
        return !isAtEnd();
    }

    public void advance() {
        char currentChar = nextChar();
        switch (currentChar) {
            case '(':
            case ')':
            case '{':
            case '}':
            case '[':
            case ']':
            case '.':
            case ',':
            case ';':
            case '+':
            case '-':
            case '*':
            case '&':
            case '|':
            case '<':
            case '>':
            case '=':
            case '~':
                addSymbol(currentChar);
                tokens.add(currentToken);
                newToken = true;
                break;
            case '/':
                // exclude comments
                if (matches('/')) {
                    while (peek() != '\n' && !isAtEnd()) nextChar();
                    newToken = false;
                } else if (matches('*')) {
                    while (!isAtEnd() && !(peek() == '*' && peekNext() == '/')) nextChar();
                    if (peek() == '*' && peekNext() == '/') {
                        nextChar();
                        nextChar();
                    }
                    newToken = false;
                } else {
                    addSymbol(currentChar);
                    tokens.add(currentToken);
                    newToken = true;
                }
                break;
            case ' ':
            case '\r':
            case '\t':
            case '\n':
                // Ignore whitespaces
                newToken = false;
                break;
            case '"':
                while (peek() != '"' && !isAtEnd()) nextChar();
                addString();
                nextChar();
                tokens.add(currentToken);
                newToken = true;
                break;
            default:
                if (isDigit(currentChar)) {
                    while (isDigit(peek())) nextChar();
                    addNumber();
                    tokens.add(currentToken);
                    newToken = true;
                } else {
                    while (isAlphanumeric(peek())) nextChar();
                    String value = source.substring(start, current);
                    try {
                        JackKeywordType keywordType = JackKeywordType.valueOf(value.toUpperCase());
                        addKeyword(keywordType);
                    } catch (IllegalArgumentException e) {
                        addIdentifier(value);
                    }
                    tokens.add(currentToken);
                    newToken = true;
                }
        }
        start = current;
    }

    public List<JackToken> generateTokens() {
        while (hasMoreTokens()) {
            advance();
        }
        return tokens;
    }

    public boolean isNewToken() {
        return newToken;
    }

    public JackToken getCurrentToken() {
        return currentToken;
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    private boolean isAlphanumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    private boolean matches(char currentChar) {
        if (isAtEnd()) {
            return false;
        }
        if (currentChar != source.charAt(current)) {
            return false;
        }
        current++;
        return true;
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private char nextChar() {
        return source.charAt(current++);
    }

    private char peek() {
        if (isAtEnd()) {
            return '\0';
        }
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) return '\0';
        return source.charAt(current + 1);
    }

    private void addSymbol(char lexeme) {
        String output = String.valueOf(lexeme);
        currentToken = new JackToken(JackTokenType.SYMBOL, output, null);
    }

    private void addString() {
        String value = source.substring(start + 1, current);
        currentToken = new JackToken(JackTokenType.STRING_CONST, value, value);
    }

    private void addNumber() {
        String substring = source.substring(start, current);
        Integer value = Integer.parseInt(substring);
        currentToken = new JackToken(JackTokenType.INT_CONST, substring, value);
    }

    private void addKeyword(JackKeywordType keywordType) {
        currentToken = new JackToken(JackTokenType.KEYWORD, keywordType.name().toLowerCase(), null);
    }

    private void addIdentifier(String value) {
        currentToken = new JackToken(JackTokenType.IDENTIFIER, value, null);
    }
}
