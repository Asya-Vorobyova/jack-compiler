public enum MemorySegment {
    CONST("constant"), LCL("local"), ARG("argument"), THIS("this"), THAT("that"), STATIC("static"),
    TEMP("temp"), POINTER("pointer");

    private String value;

    MemorySegment(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static MemorySegment fromValue(String value) {
        for (MemorySegment mem : MemorySegment.values()) {
            if (mem.getValue().equals(value)) {
                return mem;
            }
        }
        return null;
    }

    public static MemorySegment fromVariableKind(SymbolTable.VariableKind variableKind) {
        MemorySegment segment = null;
        switch (variableKind) {
            case FIELD:
                segment = MemorySegment.THIS;
                break;
            case STATIC:
                segment = MemorySegment.STATIC;
                break;
            case ARG:
                segment = MemorySegment.ARG;
                break;
            case VAR:
                segment = MemorySegment.LCL;
                break;
        }
        return segment;
    }
}
