import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JackCompiler {
    public static void main(String[] args) throws IOException {
        String inputName = args[0];
        Path inputPath = Paths.get(inputName);
        List<Path> filesToParse;
        Path outputPath;
        if (Files.isDirectory(inputPath)) {
            filesToParse = listJackFiles(inputPath);
            outputPath = inputPath;
        } else {
            filesToParse = List.of(inputPath);
            outputPath = inputPath.getParent();
        }

        for (Path path : filesToParse) {
            String fileName = path.getFileName().toString();
            int extIndex = fileName.lastIndexOf('.');
            Path fileOutputPath = Paths.get(outputPath.toString(), fileName.substring(0, extIndex) + ".xml");
            JackTokenizer tokenizer = new JackTokenizer(path);
            try (CompilationEngine engine = new CompilationEngine(tokenizer.generateTokens(), fileOutputPath)) {
                engine.compileClass();
            }
//            try (BufferedWriter writer = Files.newBufferedWriter(outputPath)) {
//                writer.write("<tokens>");
//                writer.newLine();
//                while (tokenizer.hasMoreTokens()) {
//                    tokenizer.advance();
//                    if (tokenizer.isNewToken()) {
//                        JackToken currentToken = tokenizer.getCurrentToken();
//                        String tag = currentToken.getType().getValue();
//                        writer.write(" \t<" + tag + ">");
//                        writer.write(currentToken.getLexeme());
//                        writer.write("</" + tag + ">");
//                        writer.newLine();
//                    }
//                }
//                writer.write("</tokens>");
//            }
        }
    }

    private static List<Path> listJackFiles(Path dir) throws IOException {
        try (Stream<Path> stream = Files.list(dir)) {
            return stream
                    .filter(file -> !Files.isDirectory(file) && file.toString().endsWith(".jack"))
                    .collect(Collectors.toList());
        }
    }
}
