import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class CompilationEngine implements AutoCloseable {
    private static final String CLASS_CATEGORY = "class";
    private static final String SUBROUTINE_CATEGORY = "subroutine";
    private static final String DECLARED_USAGE = "declared";
    private static final String USED_USAGE = "used";

    private final List<JackToken> tokens;
    private final VMWriter vmWriter;

    private final SymbolTable symbolTable;

    private int i = 0; // tokens' current index
    private int labelNumber = 0;

    public CompilationEngine(List<JackToken> tokens, Path outputPath) throws IOException {
        this.tokens = tokens;
        this.vmWriter = new VMWriter(outputPath);
        this.symbolTable = new SymbolTable();
    }

    public void compileClass() throws IOException {
        process(JackKeywordType.CLASS.name().toLowerCase());
        symbolTable.setClassName(tokens.get(i).getLexeme());
        if (!processIdentifier()) {
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
    }

    private void compileClassVarDec() throws IOException {
        process(JackKeywordType.STATIC.name().toLowerCase(), JackKeywordType.FIELD.name().toLowerCase());
        SymbolTable.VariableKind kind = tokens.get(i - 1).getLexeme().equals(JackKeywordType.STATIC.name().toLowerCase()) ?
                SymbolTable.VariableKind.STATIC : SymbolTable.VariableKind.FIELD;
        boolean processToken = processType() || processIdentifier();
        if (!processToken) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        String type = tokens.get(i - 1).getLexeme();
        String varName = tokens.get(i).getLexeme();
        symbolTable.define(varName, type, kind);
        if (!processIdentifier()) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        while (tokens.get(i).getLexeme().equals(",")) {
            process(",");
            varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, kind);
            if (!processIdentifier()) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            }
        }
        process(";");
    }

    private void compileSubroutineDec() throws IOException {
        symbolTable.reset();

        String subroutineType = tokens.get(i).getLexeme();
        process(JackKeywordType.CONSTRUCTOR.name().toLowerCase(), JackKeywordType.METHOD.name().toLowerCase(),
                JackKeywordType.FUNCTION.name().toLowerCase());
        boolean processToken = processType() || processIdentifier();
        if (!processToken) {
            process("void");
        }
        symbolTable.setSubroutineName(tokens.get(i).getLexeme());
        symbolTable.setSubroutineType(subroutineType);
        symbolTable.setSubroutineReturnType(tokens.get(i - 1).getLexeme());
        if (subroutineType.equals("method")) {
            symbolTable.define("this", symbolTable.getClassName(), SymbolTable.VariableKind.ARG);
        }
        processIdentifier();
        process("(");
        compileParameterList();
        process(")");
        compileSubroutineBody();
    }

    private void compileParameterList() throws IOException {
        boolean first = true;
        while (!tokens.get(i).getLexeme().equals(")")) {
            if (first) {
                first = false;
            } else {
                process(",");
            }
            boolean processToken = processType() || processIdentifier();
            if (!processToken) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            }
            String type = tokens.get(i - 1).getLexeme();
            String varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, SymbolTable.VariableKind.ARG);
            if (!processIdentifier()) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            };
        }
    }

    private void compileSubroutineBody() throws IOException {
        process("{");
        while ((tokens.get(i).getLexeme().equals(JackKeywordType.VAR.name().toLowerCase()))) {
            compileVarDec();
        }

        int nVars = symbolTable.varCount(SymbolTable.VariableKind.VAR);
        vmWriter.writeFunction(symbolTable.getClassName() + "." + symbolTable.getSubroutineName(), nVars);
        switch (symbolTable.getSubroutineType()) {
            case "method":
                vmWriter.writePush(MemorySegment.ARG, 0);
                vmWriter.writePop(MemorySegment.POINTER, 0);
                break;
            case "constructor":
                vmWriter.writePush(MemorySegment.CONST, symbolTable.varCount(SymbolTable.VariableKind.FIELD));
                vmWriter.writeCall("Memory.alloc", 1);
                vmWriter.writePop(MemorySegment.POINTER, 0);
                break;
        }

        compileStatements();
        process("}");

        vmWriter.newLine();
    }

    private void compileVarDec() throws IOException {
        process("var");
        boolean processToken = processType() || processIdentifier();
        if (!processToken) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        String type = tokens.get(i - 1).getLexeme();
        String varName = tokens.get(i).getLexeme();
        symbolTable.define(varName, type, SymbolTable.VariableKind.VAR);
        if (!processIdentifier()) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        while (tokens.get(i).getLexeme().equals(",")) {
            process(",");
            varName = tokens.get(i).getLexeme();
            symbolTable.define(varName, type, SymbolTable.VariableKind.VAR);
            if (!processIdentifier()) {
                throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
            };
        }
        process(";");
    }

    private void compileStatements() throws IOException {
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
    }

    private void compileLetStatement() throws IOException {
        process("let");

        String varName = tokens.get(i).getLexeme();
        SymbolTable.VariableKind variableKind = symbolTable.kindOf(varName);
        int index = symbolTable.indexOf(varName);
        if (!processIdentifier()) {
            throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
        }
        if (tokens.get(i).getLexeme().equals("[")) { // array access
            vmWriter.writePush(MemorySegment.fromVariableKind(variableKind), index);
            process("[");
            compileExpression();
            vmWriter.writeArithmetic(ArithmeticOp.add);
            process("]");
            process("=");
            compileExpression();
            vmWriter.writePop(MemorySegment.TEMP, 0);
            vmWriter.writePop(MemorySegment.POINTER, 1);
            vmWriter.writePush(MemorySegment.TEMP, 0);
            vmWriter.writePop(MemorySegment.THAT, 0);
            process(";");
        } else {
            process("=");
            compileExpression();
            process(";");

            if (index != -1) {
                vmWriter.writePop(MemorySegment.fromVariableKind(variableKind), index);
            }
        }
    }

    private void compileIfStatement() throws IOException {
        String label1 = generateLabel();
        String label2 = generateLabel();

        process("if");
        process("(");
        compileExpression();
        process(")");
        vmWriter.writeArithmetic(ArithmeticOp.not);
        vmWriter.writeIf(label1);
        process("{");
        compileStatements();
        process("}");
        vmWriter.writeGoto(label2);
        vmWriter.writeLabel(label1);
        if (tokens.get(i).getLexeme().equals("else")) {
            process("else");
            process("{");
            compileStatements();
            process("}");
        }
        vmWriter.writeLabel(label2);
    }

    private void compileWhileStatement() throws IOException {
        String label1 = generateLabel();
        String label2 = generateLabel();

        vmWriter.writeLabel(label1);
        process("while");
        process("(");
        compileExpression();
        process(")");
        vmWriter.writeArithmetic(ArithmeticOp.not);
        vmWriter.writeIf(label2);
        process("{");
        compileStatements();
        process("}");
        vmWriter.writeGoto(label1);
        vmWriter.writeLabel(label2);
    }

    private void compileDoStatement() throws IOException {
        process("do");

        compileExpression();
        process(";");

        vmWriter.writePop(MemorySegment.TEMP, 0);
    }

    private void compileReturnStatement() throws IOException {
        process("return");
        if (!tokens.get(i).getLexeme().equals(";")) {
            compileExpression();
        }
        process(";");

        if (symbolTable.getSubroutineReturnType().equals("void")) {
            vmWriter.writePush(MemorySegment.CONST, 0);
        }
        vmWriter.writeReturn();
    }

    private int compileExpressionList() throws IOException {
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
        return count;
    }

    private void compileExpression() throws IOException {
        compileTerm();
        while (processBinaryOperator()) {
            String operator = tokens.get(i - 1).getLexeme();
            compileTerm();
            ArithmeticOp arithmeticOp = ArithmeticOp.fromSymbol(operator);
            if (arithmeticOp != null) {
                vmWriter.writeArithmetic(Objects.requireNonNull(arithmeticOp));
            } else {
                // '*' or '/'
                vmWriter.writeMath(operator);
            }
        }
    }

    private void compileTerm() throws IOException {
        String varName = tokens.get(i).getLexeme();
        SymbolTable.VariableKind variableKind = symbolTable.kindOf(varName);
        int index = symbolTable.indexOf(varName);
        String type = symbolTable.typeOf(varName);
        String category = variableKind.name().toLowerCase();
        if (variableKind == SymbolTable.VariableKind.NONE) {
            if ((tokens.get(i + 1).getLexeme().equals("."))) {
                category = CLASS_CATEGORY;
            } else {
                category = SUBROUTINE_CATEGORY;
            }
        }
        String callMethod = tokens.get(i).getLexeme();
        int nArgs = -1;
        if (processIdentifier()) { // varName or subroutineCall or varName[...]
            switch (tokens.get(i).getLexeme()) {
                case "[":  // process array
                    if (index != -1) {
                        vmWriter.writePush(MemorySegment.fromVariableKind(variableKind), index);
                    }
                    process("[");
                    compileExpression();
                    vmWriter.writeArithmetic(ArithmeticOp.add);
                    process("]");
                    vmWriter.writePop(MemorySegment.POINTER, 1);
                    vmWriter.writePush(MemorySegment.THAT, 0);
                    break;
                case ".":  // process subroutineCall
                    if (type != null) { // call of some method of instance variable
                        callMethod = type + tokens.get(i).getLexeme() + tokens.get(i + 1).getLexeme();
                    } else { // call of a function
                        callMethod = varName + tokens.get(i).getLexeme() + tokens.get(i + 1).getLexeme();
                    }
                    process(".");
                    if (!processIdentifier()) {
                        throw new RuntimeException("Unmatched token: " + tokens.get(i).getLexeme());
                    }
                    if (variableKind != SymbolTable.VariableKind.NONE) {
                        vmWriter.writePush(MemorySegment.fromVariableKind(variableKind), index);
                    }
                    process("(");
                    nArgs = compileExpressionList();
                    if (variableKind != SymbolTable.VariableKind.NONE) {
                        nArgs++;
                    }
                    process(")");
                    vmWriter.writeCall(callMethod, nArgs);
                    break;
                case "(":  // process method call of a current class instance
                    callMethod = symbolTable.getClassName() + "." + callMethod;
                    vmWriter.writePush(MemorySegment.POINTER, 0);
                    process("(");
                    nArgs = compileExpressionList() + 1;
                    process(")");
                    vmWriter.writeCall(callMethod, nArgs);
                    break;
                default: //'pure' varName, so we push into stack
                    if (index != -1) {
                        vmWriter.writePush(MemorySegment.fromVariableKind(variableKind), index);
                    }
            }
        } else {
             if (tokens.get(i).getLexeme().equals("-") || tokens.get(i).getLexeme().equals("~")) {
                 // unaryOp term
                 String op = tokens.get(i).getLexeme();
                 parseCurrentToken();
                 compileTerm();
                 if (op.equals("-")) {
                     vmWriter.writeArithmetic(ArithmeticOp.neg);
                 } else {
                     vmWriter.writeArithmetic(ArithmeticOp.not);
                 }
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
    }

    public void close() throws IOException {
        vmWriter.close();
    }

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
        i++; // go to the next token
    }

    private boolean processIdentifier() throws IOException {
        if (tokens.get(i).getType() == JackTokenType.IDENTIFIER) {
            parseCurrentToken();
            return true;
        }
        return false;
    }

    private boolean processKeywordConstant() throws IOException {
        try {
            process(JackKeywordType.TRUE.name().toLowerCase(), JackKeywordType.FALSE.name().toLowerCase(),
                JackKeywordType.NULL.name().toLowerCase(), JackKeywordType.THIS.name().toLowerCase());
            switch (tokens.get(i - 1).getLexeme()) {
                case "true":
                    vmWriter.writePush(MemorySegment.CONST, 1);
                    vmWriter.writeArithmetic(ArithmeticOp.neg);
                    break;
                case "false":
                case "null":
                    vmWriter.writePush(MemorySegment.CONST, 0);
                    break;
                case "this":
                    vmWriter.writePush(MemorySegment.POINTER, 0);
            }
            return true;
        } catch (RuntimeException e) {
            return false;
        }
    }

    private boolean processIntegerOrString() throws IOException {
        if (tokens.get(i).getType() == JackTokenType.INT_CONST
                || tokens.get(i).getType() == JackTokenType.STRING_CONST) {
            parseCurrentToken();
            if (tokens.get(i - 1).getType() == JackTokenType.INT_CONST) {
                Integer value = (Integer) tokens.get(i - 1).getLiteral();
                vmWriter.writePush(MemorySegment.CONST, Math.abs(value));
                if (value < 0) {
                    vmWriter.writeArithmetic(ArithmeticOp.neg);
                }
            } else {
                vmWriter.pushString((String) tokens.get(i - 1).getLiteral());
            }
            return true;
        }
        return false;
    }

    private String generateLabel() {
        return symbolTable.getClassName() + "_" + labelNumber++;
    }
}
