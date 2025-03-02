import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class CompilationEngine implements AutoCloseable {
    private static final String CLASS_CATEGORY = "class";
    private static final String SUBROUTINE_CATEGORY = "subroutine";
    private static final String DECLARED_USAGE = "declared";
    private static final String USED_USAGE = "used";

    private final List<JackToken> tokens;
//    private final BufferedWriter writer;
    private final VMWriter vmWriter;

    private final SymbolTable symbolTable;

    private int i = 0; // tokens' current index

    private int indentDepth = 0;

    public CompilationEngine(List<JackToken> tokens, Path outputPath) throws IOException {
        this.tokens = tokens;
        //this.writer = Files.newBufferedWriter(outputPath);
        this.vmWriter = new VMWriter(outputPath);
        this.symbolTable = new SymbolTable();
    }

    public void compileClass() throws IOException {
//        writer.write("<class>");
//        writer.newLine();
//        indentDepth++;

        process(JackKeywordType.CLASS.name().toLowerCase());
        symbolTable.setClassName(tokens.get(i).getLexeme());
        if (!processIdentifier(CLASS_CATEGORY, DECLARED_USAGE)) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        process("{");
        while ((tokens.get(i).getLexeme().equals(JackKeywordType.STATIC.name().toLowerCase())) ||
                (tokens.get(i).getLexeme().equals(JackKeywordType.FIELD.name().toLowerCase()))) {
            compileClassVarDec();
        }
        while ((tokens.get(i).getLexeme().equals(JackKeywordType.CONSTRUCTOR.name().toLowerCase())) ||
                (tokens.get(i).getLexeme().equals(JackKeywordType.FUNCTION.name().toLowerCase())) ||
                (tokens.get(i).getLexeme().equals(JackKeywordType.METHOD.name().toLowerCase()))) {
            compileSubroutineDec();
        }
        process("}");

        indentDepth--;
//        writer.write("</class>");
//        writer.newLine();
    }

    private void compileClassVarDec() throws IOException {
//        printLine("<classVarDec>");
//        writer.newLine();
//        indentDepth++;

        process(JackKeywordType.STATIC.name().toLowerCase(), JackKeywordType.FIELD.name().toLowerCase());
        SymbolTable.VariableKind kind = tokens.get(i - 1).getLexeme().equals(JackKeywordType.STATIC.name().toLowerCase()) ?
                SymbolTable.VariableKind.STATIC : SymbolTable.VariableKind.FIELD;
        boolean processToken = processType() || processIdentifier(CLASS_CATEGORY, USED_USAGE);
        if (!processToken) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        String type = tokens.get(i - 1).getLexeme();
        String varName = tokens.get(i).getLexeme();
        symbolTable.define(varName, type, kind);
        if (!processIdentifier(kind.name().toLowerCase(), DECLARED_USAGE)) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        while (tokens.get(i).getLexeme().equals(",")) {
            process(",");
            varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, kind);
            if (!processIdentifier(kind.name().toLowerCase(), DECLARED_USAGE)) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            }
        }
        process(";");

//        indentDepth--;
//        printLine("</classVarDec>");
//        writer.newLine();
    }

    private void compileSubroutineDec() throws IOException {
//        printLine("<subroutineDec>");
//        writer.newLine();
//        indentDepth++;

        symbolTable.reset();
        symbolTable.define("this", symbolTable.getClassName(), SymbolTable.VariableKind.ARG);

        process(JackKeywordType.CONSTRUCTOR.name().toLowerCase(), JackKeywordType.METHOD.name().toLowerCase(),
                JackKeywordType.FUNCTION.name().toLowerCase());
        boolean processToken = processType() || processIdentifier(SUBROUTINE_CATEGORY, DECLARED_USAGE);
        if (!processToken) {
            process("void");
        }
        processIdentifier(SUBROUTINE_CATEGORY, DECLARED_USAGE);
        process("(");
        compileParameterList();
        process(")");
        compileSubroutineBody();

//        indentDepth--;
//        printLine("</subroutineDec>");
//        writer.newLine();
    }

    private void compileParameterList() throws IOException {
//        printLine("<parameterList>");
//        writer.newLine();
//        indentDepth++;

        boolean first = true;
        while (!tokens.get(i).getLexeme().equals(")")) {
            if (first) {
                first = false;
            } else {
                process(",");
            }
            boolean processToken = processType() || processIdentifier(CLASS_CATEGORY, USED_USAGE);
            if (!processToken) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            }
            String type = tokens.get(i - 1).getLexeme();
            String varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, SymbolTable.VariableKind.ARG);
            if (!processIdentifier(SymbolTable.VariableKind.ARG.name().toLowerCase(), DECLARED_USAGE)) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            };
        }

//        indentDepth--;
//        printLine("</parameterList>");
//        writer.newLine();
    }

    private void compileSubroutineBody() throws IOException {
//        printLine("<subroutineBody>");
//        writer.newLine();
//        indentDepth++;

        process("{");
        while ((tokens.get(i).getLexeme().equals(JackKeywordType.VAR.name().toLowerCase()))) {
            compileVarDec();
        }
        compileStatements();
        process("}");

//        indentDepth--;
//        printLine("</subroutineBody>");
//        writer.newLine();
    }

    private void compileVarDec() throws IOException {
//        printLine("<varDec>");
//        writer.newLine();
//        indentDepth++;

        process("var");
        boolean processToken = processType() || processIdentifier(CLASS_CATEGORY, USED_USAGE);
        if (!processToken) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        String type = tokens.get(i - 1).getLexeme();
        String varName = tokens.get(i).getLexeme();
        symbolTable.define(varName, type, SymbolTable.VariableKind.VAR);
        if (!processIdentifier(SymbolTable.VariableKind.VAR.name().toLowerCase(), DECLARED_USAGE)) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        while (tokens.get(i).getLexeme().equals(",")) {
            process(",");
            varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, SymbolTable.VariableKind.VAR);
            if (!processIdentifier(SymbolTable.VariableKind.VAR.name().toLowerCase(), DECLARED_USAGE)) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            };
        }
        process(";");

//        indentDepth--;
//        printLine("</varDec>");
//        writer.newLine();
    }

    private void compileStatements() throws IOException {
//        printLine("<statements>");
//        writer.newLine();
//        indentDepth++;

        boolean noMoreStatements = false;
        do {
            switch (tokens.get(i).getLexeme()) {
                case "let":
                    compileLetStatement();
                    break;
                case "if":
                    compileIfStatement();
                    break;
                case "while":
                    compileWhileStatement();
                    break;
                case "do":
                    compileDoStatement();
                    break;
                case "return":
                    compileReturnStatement();
                    break;
                default:
                    noMoreStatements = true;
                    break;
            }
        } while (!noMoreStatements);

//        indentDepth--;
//        printLine("</statements>");
//        writer.newLine();
    }

    private void compileLetStatement() throws IOException {
//        printLine("<letStatement>");
//        writer.newLine();
//        indentDepth++;

        process("let");

        String varName = tokens.get(i).getLexeme();
        SymbolTable.VariableKind variableKind = symbolTable.kindOf(varName);
        if (!processIdentifier(variableKind.name().toLowerCase(), USED_USAGE)) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        if (tokens.get(i).getLexeme().equals("[")) {
            process("[");
            compileExpression();
            process("]");
        }
        process("=");
        compileExpression();
        process(";");

//        indentDepth--;
//        printLine("</letStatement>");
//        writer.newLine();
    }

    private void compileIfStatement() throws IOException {
//        printLine("<ifStatement>");
//        writer.newLine();
//        indentDepth++;

        process("if");
        process("(");
        compileExpression();
        process(")");
        process("{");
        compileStatements();
        process("}");
        if (tokens.get(i).getLexeme().equals("else")) {
            process("else");
            process("{");
            compileStatements();
            process("}");
        }
//        indentDepth--;
//        printLine("</ifStatement>");
//        writer.newLine();
    }

    private void compileWhileStatement() throws IOException {
//        printLine("<whileStatement>");
//        writer.newLine();
//        indentDepth++;

        process("while");
        process("(");
        compileExpression();
        process(")");
        process("{");
        compileStatements();
        process("}");

//        indentDepth--;
//        printLine("</whileStatement>");
//        writer.newLine();
    }

    private void compileDoStatement() throws IOException {
//        printLine("<doStatement>");
//        writer.newLine();
//        indentDepth++;

        process("do");
        String category = SUBROUTINE_CATEGORY;
        if (tokens.get(i + 1).getLexeme().equals(".")) {
            category = CLASS_CATEGORY;
        }
        if (!processIdentifier(category, USED_USAGE)) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        if (tokens.get(i).getLexeme().equals(".")) { // process subroutineName
            process(".");
            if (!processIdentifier(SUBROUTINE_CATEGORY, USED_USAGE)) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            }
        }
        process("(");
        compileExpressionList();
        process(")");
        process(";");

//        indentDepth--;
//        printLine("</doStatement>");
//        writer.newLine();
    }

    private void compileReturnStatement() throws IOException {
//        printLine("<returnStatement>");
//        writer.newLine();
//        indentDepth++;

        process("return");
        if (!tokens.get(i).getLexeme().equals(";")) {
            compileExpression();
        }
        process(";");

//        indentDepth--;
//        printLine("</returnStatement>");
//        writer.newLine();
    }

    private int compileExpressionList() throws IOException {
//        printLine("<expressionList>");
//        writer.newLine();
//        indentDepth++;

        int count = 0;
        boolean first = true;
        while (!tokens.get(i).getLexeme().equals(")")) {
            if (first) {
                first = false;
            } else {
                process(",");
            }
            compileExpression();
            count++;
        }

//        indentDepth--;
//        printLine("</expressionList>");
//        writer.newLine();
        return count;
    }

    private void compileExpression() throws IOException {
//        printLine("<expression>");
//        writer.newLine();
//        indentDepth++;

        compileTerm();
        while (processBinaryOperator()) {
            compileTerm();
        }

//        indentDepth--;
//        printLine("</expression>");
//        writer.newLine();
    }

    private void compileTerm() throws IOException {
//        printLine("<term>");
//        writer.newLine();
//        indentDepth++;

        String varName = tokens.get(i).getLexeme();
        SymbolTable.VariableKind variableKind = symbolTable.kindOf(varName);
        String category = variableKind.name().toLowerCase();
        if (variableKind == SymbolTable.VariableKind.NONE) {
            if ((tokens.get(i + 1).getLexeme().equals("."))) {
                category = CLASS_CATEGORY;
            } else {
                category = SUBROUTINE_CATEGORY;
            }
        }
        if (processIdentifier(category, USED_USAGE)) { // varName or subroutineCall or varName[...]
            switch (tokens.get(i).getLexeme()) {
                case "[":  // process array
                    process("[");
                    compileExpression();
                    process("]");
                    break;
                case ".":  // process subroutineCall
                    process(".");
                    if (!processIdentifier(SUBROUTINE_CATEGORY, USED_USAGE)) {
                        throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
                    }
                    process("(");
                    compileExpressionList();
                    process(")");
                    break;
                case "(":  // process method call
                    process("(");
                    compileExpressionList();
                    process(")");
                    break;
            }
        } else {
             if (tokens.get(i).getLexeme().equals("-") || tokens.get(i).getLexeme().equals("~")) {
                 // unaryOp term
                 parseCurrentToken();
                 compileTerm();
             } else if (tokens.get(i).getLexeme().equals("(")) {
                 // ( expression )
                 process("(");
                 compileExpression();
                 process(")");
             } else {
                 if (!processKeywordConstant() && !processIntegerOrString()) {
                     throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
                 }
             }

        }

//        indentDepth--;
//        printLine("</term>");
//        writer.newLine();
    }

    public void close() throws IOException {
//        writer.close();
        vmWriter.close();
    }

//    private void printLine(String str) throws IOException {
//        for (int i = 0; i < indentDepth; i++) {
//            writer.write('\t');
//        }
//        writer.write(str);
//    }

    private void process(String... token) throws IOException {
        for (String str : token) {
            if (tokens.get(i).getLexeme().equals(str)) {
                parseCurrentToken();
                return;
            }
        }
        throw new RuntimeException("Unmatched token(s): " + Arrays.toString(token));
    }

    private boolean processBinaryOperator() throws IOException {
        String[] operators = { "+", "-", "*", "/", "&", "|", "<", ">", "=" };
        for (String str : operators) {
            if (tokens.get(i).getLexeme().equals(str)) {
                parseCurrentToken();
                return true;
            }
        }
        return false;
    }

    private boolean processType() throws IOException {
        String[] types = { JackKeywordType.INT.name().toLowerCase(), JackKeywordType.CHAR.name().toLowerCase(),
                JackKeywordType.BOOLEAN.name().toLowerCase() };
        for (String str : types) {
            if (tokens.get(i).getLexeme().equals(str)) {
                parseCurrentToken();
                return true;
            }
        }
        return false;
    }

    private void parseCurrentToken() throws IOException {
        JackToken currentToken = tokens.get(i);
        String tag = currentToken.getType().getValue();
//        printLine("<" + tag + ">");
//        writer.write(escapeSymbol(currentToken.getLexeme()));
//        writer.write("</" + tag + ">");
//        writer.newLine();
        i++; // go to the next token
    }

    private void parseCurrentIdentifier(String category, String usage) throws IOException {
        JackToken currentToken = tokens.get(i);
        int index = symbolTable.indexOf(currentToken.getLexeme());
        String tag = currentToken.getType().getValue();
//        printLine("<" + tag + ">");
//        writer.newLine();
//        indentDepth++;
//        printLine("<name>");
//        writer.write(currentToken.getLexeme());
//        writer.write("</name>");
//        writer.newLine();
//        printLine("<category>");
//        writer.write(category);
//        writer.write("</category>");
//        writer.newLine();
//        if (index != -1) {
//            printLine("<index>");
//            writer.write(Integer.toString(index));
//            writer.write("</index>");
//            writer.newLine();
//        }
//        printLine("<usage>");
//        writer.write(usage);
//        writer.write("</usage>");
//        writer.newLine();
//        indentDepth--;
//        printLine("</" + tag + ">");
//        writer.newLine();
        i++; // go to the next token
    }

    private boolean processIdentifier(String category, String usage) throws IOException {
        if (tokens.get(i).getType() == JackTokenType.IDENTIFIER) {
            parseCurrentIdentifier(category, usage);
            return true;
        }
        return false;
    }

    private boolean processKeywordConstant() throws IOException {
        try {
            process(JackKeywordType.TRUE.name().toLowerCase(), JackKeywordType.FALSE.name().toLowerCase(),
                JackKeywordType.NULL.name().toLowerCase(), JackKeywordType.THIS.name().toLowerCase());
            return true;
        } catch (RuntimeException e) {
            return false;
        }
    }

    private boolean processIntegerOrString() throws IOException {
        if (tokens.get(i).getType() == JackTokenType.INT_CONST
                || tokens.get(i).getType() == JackTokenType.STRING_CONST) {
            parseCurrentToken();
            return true;
        }
        return false;
    }

    String escapeSymbol(String lexeme) {
        String output = lexeme;
        switch (lexeme) {
            case "<": output = "&lt;"; break;
            case ">": output = "&gt;"; break;
            case "&": output = "&amp;"; break;
            case "\"": output = "&quot;"; break;
        }
        return output;
    }

    private enum JackIdentifierUsage {
        DECLARED, USED
    }
}
