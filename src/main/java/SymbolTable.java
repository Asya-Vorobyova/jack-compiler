import java.util.HashMap;

public class SymbolTable {
    private final HashMap<String, VariableInfo> classSymbolTable;
    private final HashMap<String, VariableInfo> subroutineSymbolTable;
    private String className;
    private int fieldCount, staticCount, argCount, localCount;

    public SymbolTable() {
        classSymbolTable = new HashMap<>();
        subroutineSymbolTable = new HashMap<>();
        fieldCount = 0;
        staticCount = 0;
        argCount = 0;
        localCount = 0;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getClassName() {
        return className;
    }

    public void reset() {
        subroutineSymbolTable.clear();
        argCount = 0;
        localCount = 0;
    }

    public void define(String name, String type, VariableKind kind) {
        assert kind != null;
        assert name != null && !name.isEmpty();
        assert type != null && !type.isEmpty();
        switch (kind) {
            case FIELD:
                classSymbolTable.put(name, new VariableInfo(type, kind, fieldCount++));
                break;
            case STATIC:
                classSymbolTable.put(name, new VariableInfo(type, kind, staticCount++));
                break;
            case ARG:
                subroutineSymbolTable.put(name, new VariableInfo(type, kind, argCount++));
                break;
            case VAR:
                subroutineSymbolTable.put(name, new VariableInfo(type, kind, localCount++));
            default:
                break;
        }
    }

    public int varCount(VariableKind kind) {
        int count = 0;
        switch (kind) {
            case FIELD:
                count = fieldCount;
                break;
            case STATIC:
                count = staticCount;
                break;
            case ARG:
                count = argCount;
                break;
            case VAR:
                count = localCount;
                break;
            default:
                break;
        }
        return count;
    }

    public VariableKind kindOf(String name) {
        assert name != null && !name.isEmpty();
        if (subroutineSymbolTable.containsKey(name)) {
            return subroutineSymbolTable.get(name).getKind();
        } else if (classSymbolTable.containsKey(name)) {
            return classSymbolTable.get(name).getKind();
        } else {
           return VariableKind.NONE;
        }
    }

    public String typeOf(String name) {
        assert name != null && !name.isEmpty();
        if (subroutineSymbolTable.containsKey(name)) {
            return subroutineSymbolTable.get(name).getType();
        } else if (classSymbolTable.containsKey(name)) {
            return classSymbolTable.get(name).getType();
        } else {
            return null;
        }
    }

    public int indexOf(String name) {
        assert name != null && !name.isEmpty();
        if (subroutineSymbolTable.containsKey(name)) {
            return subroutineSymbolTable.get(name).getIndex();
        } else if (classSymbolTable.containsKey(name)) {
            return classSymbolTable.get(name).getIndex();
        } else {
            return -1;
        }
    }

    private static class VariableInfo {
        private String type;
        private VariableKind kind;
        private int index;

        VariableInfo(String type, VariableKind kind, int index) {
            this.type = type;
            this.kind = kind;
            this.index = index;
        }

        String getType() {
            return type;
        }

        VariableKind getKind() {
            return kind;
        }

        int getIndex() {
            return index;
        }
    }

    public enum VariableKind {
        FIELD, STATIC, ARG, VAR, NONE
    }
}
