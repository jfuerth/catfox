///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS info.picocli:picocli:4.7.6

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

@Command
public class basic {

    @Option(names = "--debug")
    boolean debug;

    public static void main(String... args) throws IOException {
        int result = new CommandLine(new basic())
                .execute(args);
        System.exit(result);
    }

    @Command(name = "print")
    void printFile(
        @Parameters(index = "0", description="File to read") File file
    ) throws IOException {
        InputStream in = new FileInputStream(file);
        Memory mem = new Memory();

        int loadAddr = readWord(in);
        debugf("Loading at %x%n", loadAddr);
        int len = in.read(mem.mem, loadAddr, 65536 - loadAddr);
        debugf("Loaded %d bytes%n", len);

        MemScanner scanner = new MemScanner(mem, loadAddr);

        int nextLineAddr = 0;
        while(true) {
            nextLineAddr = scanner.readWord();
            if (nextLineAddr == 0) break;

            int lineNum = scanner.readWord();
            System.out.printf("%d ", lineNum);
            int ch;
            while ((ch = scanner.readByte()) != 0) {
                // System.out.printf("Next token $%x%n", ch);
                if (BT.isToken(ch)) {
                    System.out.print(BT.ofToken(ch).str);
                } else {
                    System.out.print((char) ch);
                }
            }
            System.out.println();

            scanner.seek(nextLineAddr);
            debugf("Next line at %x%n", nextLineAddr);
        }
    }

    @Command(name = "make-loader")
    void makeLoader(
        @Option(names="-o", description="Output file") File outputFile,
        @Parameters(description="Files to load (in order)") String[] loadFiles
    ) throws IOException {
        Memory mem = new Memory();
        int startAddr = 0x0801;
        BasicMemWriter writer = new BasicMemWriter(mem, startAddr, 0, 1);
        writer.writeBasicLine(
            "I", BT.OP_EQ, "I", BT.OP_PLUS, "1",
            ":", BT.ON, "I", BT.GOTO,
            IntStream.range(1, loadFiles.length + 1)
               .mapToObj(String::valueOf)
               .collect(Collectors.joining(",")));

        for (String filename : loadFiles) {
            writer.writeBasicLine(BT.LOAD, "\"" + filename + "\",8,1");
        }
        writer.endProgram();
        try (OutputStream out = new java.io.FileOutputStream(outputFile)) {
            out.write(startAddr & 0xff);
            out.write((startAddr >> 8) & 0xff);
            out.write(mem.mem, startAddr, writer.ptr - startAddr);
        }
    }

    void debugf(String fmt, Object ... args) {
        if (debug) {
            System.err.printf(fmt, args);
        }
    }

    /** The BASIC Tokens */
    enum BT {
        END(0x80, "END"),
        FOR(0x81, "FOR"),
        NEXT(0x82, "NEXT"),
        DATA(0x83, "DATA"),
        INPUT_NUM(0x84, "INPUT#"),
        INPUT(0x85, "INPUT"),
        DIM(0x86, "DIM"),
        READ(0x87, "READ"),
        LET(0x88, "LET"),
        GOTO(0x89, "GOTO"),
        RUN(0x8A, "RUN"),
        IF(0x8B, "IF"),
        RESTORE(0x8C, "RESTORE"),
        GOSUB(0x8D, "GOSUB"),
        RETURN(0x8E, "RETURN"),
        REM(0x8F, "REM"),
        STOP(0x90, "STOP"),
        ON(0x91, "ON"),
        WAIT(0x92, "WAIT"),
        LOAD(0x93, "LOAD"),
        SAVE(0x94, "SAVE"),
        VERIFY(0x95, "VERIFY"),
        DEF(0x96, "DEF"),
        POKE(0x97, "POKE"),
        PRINT_NUM(0x98, "PRINT#"),
        PRINT(0x99, "PRINT"),
        CONT(0x9A, "CONT"),
        LIST(0x9B, "LIST"),
        CLR(0x9C, "CLR"),
        CMD(0x9D, "CMD"),
        SYS(0x9E, "SYS"),
        OPEN(0x9F, "OPEN"),
        CLOSE(0xA0, "CLOSE"),
        GET(0xA1, "GET"),
        NEW(0xA2, "NEW"),
        TAB(0xA3, "TAB("),
        TO(0xA4, "TO"),
        FN(0xA5, "FN"),
        SPC(0xA6, "SPC("),
        THEN(0xA7, "THEN"),
        NOT(0xA8, "NOT"),
        STEP(0xA9, "STEP"),
        OP_PLUS(0xAA, "+"),
        OP_MINUS(0xAB, "-"),
        OP_MULT(0xAC, "*"),
        OP_DIVIDE(0xAD, "/"),
        OP_POW(0xAE, "↑"),
        AND(0xAF, "AND"),
        OR(0xB0, "OR"),
        OP_GT(0xB1, ">"),
        OP_EQ(0xB2, "="),
        OP_LT(0xB3, "<"),
        SGN(0xB4, "SGN"),
        INT(0xB5, "INT"),
        ABS(0xB6, "ABS"),
        USR(0xB7, "USR"),
        FRE(0xB8, "FRE"),
        POS(0xB9, "POS"),
        SQR(0xBA, "SQR"),
        RND(0xBB, "RND"),
        LOG(0xBC, "LOG"),
        EXP(0xBD, "EXP"),
        COS(0xBE, "COS"),
        SIN(0xBF, "SIN"),
        TAN(0xC0, "TAN"),
        ATN(0xC1, "ATN"),
        PEEK(0xC2, "PEEK"),
        LEN(0xC3, "LEN"),
        STR$(0xC4, "STR$"),
        VAL(0xC5, "VAL"),
        ASC(0xC6, "ASC"),
        CHR$(0xC7, "CHR$"),
        LEFT$(0xC8, "LEFT$"),
        RIGHT$(0xC9, "RIGHT$"),
        MID$(0xCA, "MID$"),
        GO(0xCB, "GO");

        private int token;
        private String str;

        private BT(int token, String str) {
            this.token = token;
            this.str = str;
        }

        static BT ofToken(int token) {
            for (BT t : values()) {
                if (t.token == token) {
                    return t;
                }
            }
            throw new IllegalArgumentException("No such token " + token);
        }

        static boolean isToken(int ch) {
            return ch >= END.token;
        }
    }

    static int readWord(InputStream in) throws IOException {
        int low = in.read();
        int high = in.read();
        return ((high << 8) & 0xff00) | (low & 0xff);
    }

    static class Memory {
        byte[] mem = new byte[65536];

        int word(int addr) {
            int low = mem[addr];
            int high = mem[addr + 1];
            return ((high << 8) & 0xff00) | (low & 0xff);
        }

        public void writeWord(int addr, int w) {
            mem[addr] = (byte) (w & 0xff);
            mem[addr+1] = (byte) ((w >> 8) & 0xff);
        }
    }

    static class MemScanner {
        Memory mem;
        int ptr;

        MemScanner(Memory mem, int startAddr) {
            this.mem = mem;
            this.ptr = startAddr;
        }

        int readWord() {
            int w = mem.word(ptr);
            ptr += 2;
            return w;
        }

        int readByte() {
            int b = mem.mem[ptr];
            ptr += 1;
            return b & 0xff;
        }

        void seek(int addr) {
            ptr = addr;
        }
    }

    static class BasicMemWriter {
        Memory mem;
        
        /** Pointer to the next memory location to write */
        int ptr;

        /** Line number of the next line in the program */
        int line;

        /** Amount to increment successive lines by */
        private int lineStep;

        BasicMemWriter(Memory mem, int startAddr, int startLineNumber, int lineStep) {
            this.mem = mem;
            this.ptr = startAddr;
            this.line = startLineNumber;
            this.lineStep = lineStep;
        }

        void writeBasicLine(Object ... chunks) {
            // make space for the pointer to the line after this
            int nextLinePtr = ptr;
            writeWord(-1); // will update at end of line

            writeWord(line);
            line += lineStep;

            for (Object chunk : chunks) {
                if (chunk instanceof BT token) {
                    writeByte(token.token);
                } else if (chunk instanceof String str) {
                    writeString(str);
                }
            }

            // terminate line
            writeByte(0);

            // update the next line pointer now that we know where it goes
            mem.writeWord(nextLinePtr, ptr);
        }

        public void endProgram() {
            writeByte(0);
            writeByte(0);
            writeByte(0);
        }

        void writeWord(int w) {
            mem.writeWord(ptr, w);
            ptr += 2;
        }

        void writeByte(int b) {
            mem.mem[ptr] = (byte) (b & 0xff);
            ptr += 1;
        }

        void writeString(String s) {
            for (char ch : s.toCharArray()) {
                writeByte(ch);
            }
        }

        void seek(int addr) {
            ptr = addr;
        }
    }

}
