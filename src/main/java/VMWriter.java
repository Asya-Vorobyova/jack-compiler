import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class VMWriter implements AutoCloseable {
    private final BufferedWriter writer;

    public VMWriter(Path outputPath) throws IOException {
        this.writer = Files.newBufferedWriter(outputPath);
    }

    public void newLine() throws IOException {
        writer.newLine();
    }

    public void writePush(MemorySegment segment, int index) throws IOException {
        oneLine("push " + segment.getValue() + " " + index);
    }

    public void writePop(MemorySegment segment, int index) throws IOException {
        oneLine("pop " + segment.getValue() + " " + index);
    }

    public void writeArithmetic(ArithmeticOp op) throws IOException {
        oneLine(op.toString());
    }

    public void writeLabel(String label) throws IOException {
        oneLine("label " + label);
    }

    public void writeGoto(String label) throws IOException {
        oneLine("goto " + label);
    }

    public void writeIf(String label) throws IOException {
        oneLine("if-goto " + label);
    }

    public void writeCall(String name, int nArgs) throws IOException {
        oneLine("call " + name + " " + nArgs);
    }

    public void writeFunction(String name, int nVars) throws IOException {
        oneLine("function " + name + " " + nVars);
    }

    public void writeReturn() throws IOException {
        oneLine("return");
    }

    @Override
    public void close() throws IOException {
        writer.close();
    }

    private void oneLine(String line) throws IOException {
        writer.write(line);
        writer.newLine();
    }
}
