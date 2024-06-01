///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS com.fasterxml.jackson.core:jackson-databind:2.17.1
//DEPS info.picocli:picocli:4.7.6

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.core.exc.StreamReadException;
import com.fasterxml.jackson.databind.DatabindException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

import picocli.CommandLine;
import picocli.CommandLine.IExecutionExceptionHandler;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ParseResult;

public class pexplode {

    static final ObjectMapper objectMapper = JsonMapper.builder()
            .enable(StreamReadFeature.INCLUDE_SOURCE_IN_LOCATION)
            .findAndAddModules()
            .build();

    public static void main(String... args) throws StreamReadException, DatabindException, IOException {
        int result = new CommandLine(new PetsciiEditExploder())
                .setExecutionExceptionHandler(new UserFacingExceptionHandler())
                .execute(args);
        System.exit(result);
    }

    static class PetsciiEditExploder implements Runnable {

        @Option(names = {
                "--charsets" }, defaultValue = "*", description = "Names of charsets to process. Glob matching is supported.")
        Set<String> charsetGlobs;

        @CommandLine.Option(names = { "-l", "--load-addr" }, description = """
                Set or override load address (4-digit hex) for an asset.
                The asset name is a glob, matched case-insensitively by name.
                This option overrides any load address specified in a part's name.
                For screens, this sets the load address of the character matrix.
                Use --color-load-address to set the load address of the colour matrix.
                """)
        Map<String, String> loadAddresses = new LinkedHashMap<>();

        @CommandLine.Option(names = { "-c", "--color-load-addr", "--colour-load-addr" }, description = """
                Set or override load address (4-digit hex) for a screen's colour matrix.
                All colour matrix load addresses otherwise default to $d800.
                """)
        Map<String, String> colorLoadAddresses = new LinkedHashMap<>();

        @Parameters(description = "PETSCII Editor (.pe) file to read")
        Path inputFile;

        @Override
        public void run() {
            downcaseKeys(loadAddresses);
            downcaseKeys(colorLoadAddresses);

            PetsciiProject peProject;
            try {
                PetsciiProjectEnvelope envelope = objectMapper.readValue(inputFile.toFile(),
                        PetsciiProjectEnvelope.class);
                peProject = objectMapper.readValue(lzwDecompress(envelope.data()), PetsciiProject.class);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            // output charsets
            for (CharsetBitmaps cs : peProject.charsets()) {
                if (matchesAnyGlob(cs.name(), charsetGlobs))
                    writePrgFile(
                            cs.name() + ".prg",
                            determineLoadAddress(cs.name()),
                            flatten(cs.bitmaps()));
            }

            // output screens
            for (Screen s : peProject.screens()) {
                byte[] charData = flatten(s.charData());
                byte[] colourData = flatten(s.colorData());
                writeCrunchedFile(s.name(), Map.of(
                        determineLoadAddress(s.name), charData,
                        determineColorLoadAddress(s.name), cleanColourData(charData, colourData)));
            }

            // output sprites
            // at this point, we want all sprites mashed consecutively into one file,
            // and a companion include file that defines sprite numbers, which is
            // [address within vic bank] / 64.
            // Sometimes you'd want something else, for example if you need to load some
            // sprites separately for different screens or phases of the game.
            // Parameterizing all of this is possible but maybe not worth it at this stage
            // since the eventual requirements aren't known.
            final int firstSpriteAddr = 0x4c00;
            final String spriteSetName = "sprites";
            final File spritenumsFile = new File(spriteSetName + "_nums.s");
            final File spriteDataFile = new File(spriteSetName + ".prg");

            int nextSpriteAddr = firstSpriteAddr;
            try (PrintWriter spriteNumsOut = new PrintWriter(spritenumsFile);
                    OutputStream dataOut = new FileOutputStream(spriteDataFile)) {
                
                // load address
                writeWord(dataOut, nextSpriteAddr);

                spriteNumsOut.printf("%s_first=%d%n", spriteSetName, vicSpriteNum(nextSpriteAddr));
                for (SpriteSet ss : peProject.spriteSets()) {
                    for (int i = 0; i < ss.sprites().size(); i++) {
                        Sprite s = ss.sprites().get(i);
                        int numberInUi = i + 1;
                        String name = ss.name() + "_" + numberInUi;

                        spriteNumsOut.printf("%s=%d ; addr=%04x%n", name, vicSpriteNum(nextSpriteAddr), nextSpriteAddr);

                        for (int[] spriteBits : s.bitmapData) {
                            byte b = condenseBits(spriteBits);
                            dataOut.write(b);
                        }
                        dataOut.write((byte) 0); // pad to 64 bytes

                        nextSpriteAddr += 64;
                    }
                }
                spriteNumsOut.printf("%s_last=%d%n", spriteSetName, vicSpriteNum(nextSpriteAddr));
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        /** Returns the VIC-II sprite number for the given address */
        private int vicSpriteNum(int nextSpriteAddr) {
            return (nextSpriteAddr & 0x3fff) / 64;
        }

        /**
         * Takes an 8-element array of 0s and 1s (MSB first) and returns the corresponding byte.
         */
        private byte condenseBits(int[] spriteBits) {
            if (spriteBits.length != 8) {
                throw new IllegalArgumentException("spriteBits.length is " + spriteBits.length + "; expected 8");
            }
            int b = 0;
            for (int i = 0; i < 8; i++) {
                int v = spriteBits[i];
                if (v != 0 && v != 1) {
                    throw new IllegalArgumentException("spriteBits[" + i + "] is " + spriteBits[i] + "; expected 1 or 0");
                }
                b = b << 1;
                b |= v;
            }
            return (byte) (b & 0xff);
        }

        static class OutputMode {
        }

        private void downcaseKeys(Map<String, ?> map) {
            map.keySet().forEach(String::toLowerCase);
        }

        private static boolean matchesAnyGlob(String value, Collection<String> globs) {
            // convert globs to regex
            StringBuilder sb = new StringBuilder();
            sb.append("(");
            for (String glob : globs) {
                if (sb.length() > 1) {
                    sb.append("|");
                }
                String[] parts = glob.splitWithDelimiters("[*?]", 0);
                for (int i = 0; i < parts.length; i++) {
                    // add the literal part
                    if (parts[i].length() > 0) {
                        sb.append("\\Q").append(parts[i]).append("\\E");
                    }
                    // add the wildcard part (unless at end of input)
                    if (++i < parts.length) {
                        sb.append(switch (parts[i]) {
                            case "*" -> ".*";
                            case "?" -> ".";
                            default -> throw new AssertionError("Unexpected glob char " + parts[i]);
                        });
                    }
                }
            }
            sb.append(")");
            return value.matches(sb.toString());
        }

        private static final Pattern PART_NAME_LOAD_ADDRESS_PATTERN = Pattern.compile("\\$([0-9a-fA-F]+)");

        private int determineLoadAddress(String name) {
            return determineLoadAddress(name, loadAddresses, null);
        }

        private int determineColorLoadAddress(String name) {
            return determineLoadAddress(name, colorLoadAddresses, 0xd800);
        }

        private static int determineLoadAddress(String name, Map<String, String> nameMap, Integer defaultValue) {
            Matcher partNameMatcher = PART_NAME_LOAD_ADDRESS_PATTERN.matcher(name);
            String override = getByGlob(nameMap, name.toLowerCase());
            if (override != null) {
                return Integer.valueOf(override, 16);
            } else if (partNameMatcher.find()) {
                return Integer.valueOf(partNameMatcher.group(1), 16);
            } else if (defaultValue != null) {
                return defaultValue;
            }
            throw new UserFacingException(
                    "Can't determine load address for \"%s\"".formatted(name),
                    """
                            Either add a load address to the name of the part (like \"%s $a000\")
                            or specify it on the command line with --load-addr='%s=a000'.
                            """.formatted(name, name));
        }

        /**
         * Finds a value in a map whose keys are globs.
         * 
         * @param m          The map. Keys are treated as globs.
         * @param literalKey The key to match against the globs in the map's key set.
         * @return The value from the first mapping that matches the given literal key.
         */
        private static String getByGlob(Map<String, String> m, String literalKey) {
            for (var entry : m.entrySet()) {
                if (matchesAnyGlob(literalKey, Set.of(entry.getKey()))) {
                    return entry.getValue();
                }
            }
            return null;
        }
    }

    /**
     * Sets colour codes for spaces to the colour code of the cell to the left.
     * This optimizes for run-length encoding by eliminating random colour
     * variations where they can't be seen.
     * <p>
     * It's assumed that the space character is 0x20.
     * 
     * @return a cleaned version of the colour data.
     */
    private static byte[] cleanColourData(byte[] charData, byte[] colourData) {
        byte[] cleaned = new byte[colourData.length];
        for (int i = 0; i < charData.length; i++) {
            if (i > 0 && charData[i] == 0x20) {
                cleaned[i] = cleaned[i - 1];
            } else {
                cleaned[i] = colourData[i];
            }
        }
        return cleaned;
    }

    private static byte[] flatten(List<int[]> intArrays) {
        byte result[] = new byte[intArrays.stream().mapToInt(a -> a.length).sum()];
        int i = 0;
        for (int[] array : intArrays) {
            for (int v : array) {
                result[i++] = (byte) (v & 0xff);
            }
        }
        ;
        return result;
    }

    private static void writeWord(OutputStream out, int word) throws IOException {
        out.write((byte) (word & 0xff));
        out.write((byte) (word >> 8 & 0xff));
    }

    private static void writeCrunchedFile(String filename, Map<Integer, byte[]> segments) {
        System.err.printf("Writing %s%n", filename);
        try (OutputStream out = new FileOutputStream(filename)) {
            for (var segment : segments.entrySet()) {
                int addr = segment.getKey();
                byte[] rawBytes = segment.getValue();

                writeWord(out, addr);
                byte[] crunchedBytes = crunch(rawBytes);
                out.write(crunchedBytes);

                // end segment
                out.write((byte) 0xff);
                out.write((byte) 0x00);

                System.err.printf("  $%x - %d -> %d (%.2f%%)%n",
                        addr, rawBytes.length, crunchedBytes.length,
                        ((float) crunchedBytes.length) / ((float) rawBytes.length) * 100f);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static byte[] crunch(byte[] rawBytes) {
        ArrayList<Byte> compressed = new ArrayList<>();
        for (int i = 0; i < rawBytes.length;) {
            int runlen = findRun(rawBytes, i);
            Similarity repeat = findSim(rawBytes, i);

            if (repeat.length() > 3 && repeat.length() > runlen) {
                System.err.printf("Using sim length %d start %d: %s%n", repeat.length(), repeat.start(),
                        sliceToString(rawBytes, repeat.start(), repeat.length()));
                compressed.add((byte) 0xfe);
                compressed.add((byte) repeat.length());
                compressed.add((byte) (i - repeat.start()));
                i += repeat.length();
            } else if (runlen > 2) {
                compressed.add((byte) 0xff);
                compressed.add((byte) runlen);
                compressed.add((byte) rawBytes[i]);
                i += runlen;
            } else if (rawBytes[i] >= 0xfe) {
                compressed.add((byte) 0xff);
                compressed.add((byte) 1);
                compressed.add((byte) rawBytes[i]);
                i += 1;
            } else {
                compressed.add((byte) rawBytes[i]);
                i += 1;
            }
            verifyNoStopCodes(rawBytes, i, compressed, runlen, repeat);
        }
        byte result[] = new byte[compressed.size()];
        for (int i = 0; i < compressed.size(); i++) {
            result[i] = (byte) (compressed.get(i) & 0xff);
        }
        return result;
    }

    private static void verifyNoStopCodes(
            byte[] rawBytes,
            int i,
            ArrayList<Byte> compressed,
            int runlen,
            pexplode.Similarity repeat) {
        int stopCodeIndex = Collections.indexOfSubList(
                compressed,
                List.of(
                        Byte.valueOf((byte) 0xff),
                        Byte.valueOf((byte) 0)));
        if (stopCodeIndex != -1) {
            throw new AssertionError("Found stop code at index %d. i=%d, runlen=%d, repeat=%s"
                    .formatted(stopCodeIndex, i, runlen, repeat));
        }
    }

    private static Object sliceToString(byte[] rawBytes, int start, int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = start; start + i < length; i++) {
            int ch = rawBytes[i];
            if (ch < ' ' || ch > '~') {
                sb.append('?');
            } else {
                sb.append((char) ch);
            }
        }
        return sb.toString();
    }

    /**
     * Returns the length of the run starting at i, up to the maximum
     * length 255.
     */
    private static int findRun(byte[] rawBytes, int i) {
        final int startPos = i;
        int ch = rawBytes[i];
        do {
            i++;
        } while (i - startPos < 255
                && i < rawBytes.length
                && rawBytes[i] == ch);
        return i - startPos;
    }

    record Similarity(int start, int length) {

        public static final Similarity NONE = new Similarity(0, 0);

        public Similarity {
            if (length < 0 || length > 255)
                throw new AssertionError("Length %d out of range".formatted(length));
            if (start < 0)
                throw new AssertionError("Start %d out of range".formatted(start));
        }

        public boolean isLongerThan(Similarity other) {
            return this.length > other.length;
        }
    }

    /**
     * Finds the maximal sequence of bytes starting startIdx which is a repeat of
     * bytes starting at some earlier index.
     * <p>
     * The similarity can overlap regions past startIdx, but it will always start
     * at least one character behind (or else the decoder would need to predict
     * the future!):
     * <p>
     * Example 1: overlap
     * 
     * <pre>
     * In:   AAAABBCDDDAAAABBCDDD
     *        ^ startIdx = 1
     * 
     * Out:  ^ start = 0
     *           ^ length = 3
     * </pre>
     * 
     * Example 2: no overlap
     * 
     * <pre>
     * In:   AAAABBCDDDAAAABBCDDD
     *                 ^ startIdx = 10
     * 
     * Out:  ^ start = 0
     *                 ^ length = 10
     * </pre>
     * 
     * @param rawBytes The array to search for a similar subsequence within.
     * @param startIdx The start index for the search
     * @return a similarity that points to the start index of the sequence that's
     *         repeated at startIdx, and its length. The similar sequence will start
     *         no
     *         more than 255 characters before startIdx, and will have length no
     *         more than
     *         255.
     */
    private static pexplode.Similarity findSim(byte[] rawBytes, final int startIdx) {
        Similarity bestSim = Similarity.NONE;
        for (int i = Math.max(startIdx - 255, 0); i < startIdx; i++) {
            int r = i; // rear pointer
            int f = startIdx; // forward pointer
            while (f < rawBytes.length && rawBytes[r] == rawBytes[f] && f - startIdx < 255) {
                r++;
                f++;
            }
            Similarity sim = new Similarity(i, f - startIdx);
            if (sim.isLongerThan(bestSim)) {
                bestSim = sim;
            }
        }
        return bestSim;
    }

    static void writePrgFile(String filename, int loadAddress, byte[] bytes) {
        System.err.printf("Writing %s ($%x)%n", filename, loadAddress);
        try (OutputStream out = new FileOutputStream(filename)) {
            writeWord(out, loadAddress);
            out.write(bytes);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static record PetsciiProjectEnvelope(
            /** "PETSCII Editor" */
            String app,

            /** "3.0" */
            String version,

            /** LZW compressed JSON string */
            String data) {
    }

    static record PetsciiProject(
            String app,
            String url,
            Object meta,
            Object options,
            Object clipboards,
            List<CharsetBitmaps> charsets,
            List<Screen> screens,
            List<SpriteSet> spriteSets) {
    }

    static record CharsetBitmaps(
            String name,
            String mode,
            byte bgColor,
            byte charColor,
            byte multiColor1,
            byte multiColor2,
            List<int[]> bitmaps) {
    }

    static record Screen(
            String name,
            String mode,
            int sizeX,
            int sizeY,
            byte colorBorder,
            byte colorBg,
            byte colorChar,
            byte multiColor1,
            byte multiColor2,
            byte extBgColor1,
            byte extBgColor2,
            byte extBgColor3,
            byte spriteMultiColor1,
            byte spriteMultiColor2,
            String spritesInBorder,
            boolean spritesVisible,
            int characterSet,
            List<int[]> charData,
            List<int[]> colorData,
            List<Sprite> sprites,
            List<Object> undoStack,
            List<Object> redoStack) {
    }

    static record SpriteSet(
            String name,
            List<Sprite> sprites,
            List<Object> undoStack,
            List<Object> redoStack) {
    }

    static record Sprite(
            String uid,
            String mode,
            byte colorBg,
            byte colorSprite,
            byte multiColor1,
            byte multiColor2,
            boolean expandX,
            boolean expandY,
            List<int[]> bitmapData) {
    }

    // chatgpt helped me figure this out
    public static String lzwDecompress(String input) {
        HashMap<Integer, String> dictionary = new HashMap<>();
        StringBuilder decompressed = new StringBuilder();
        int nextCode = 256;

        // Initialize dictionary with ASCII characters
        for (int i = 0; i < 256; i++) {
            dictionary.put(i, String.valueOf((char) i));
        }

        String currentCode = dictionary.get((int) input.charAt(0));
        decompressed.append(currentCode);

        for (int i = 1; i < input.length(); i++) {
            int code = (int) input.charAt(i);

            String entry;
            if (dictionary.containsKey(code)) {
                entry = dictionary.get(code);
            } else if (code == nextCode) {
                entry = currentCode + currentCode.charAt(0);
            } else {
                throw new IllegalArgumentException("Invalid compressed input.");
            }

            decompressed.append(entry);
            dictionary.put(nextCode++, currentCode + entry.charAt(0));
            currentCode = entry;
        }

        return decompressed.toString();
    }

    static class UserFacingException extends RuntimeException {
        private final String advice;

        public UserFacingException(String message, String advice) {
            super(message);
            this.advice = advice;
        }

        public String advice() {
            return advice;
        }
    }

    static class UserFacingExceptionHandler implements IExecutionExceptionHandler {
        public int handleExecutionException(Exception ex, CommandLine cmd, ParseResult parseResult) {
            if (ex instanceof UserFacingException ufex) {
                cmd.getErr().println(cmd.getColorScheme().errorText("Error: " + ex.getMessage()));
                cmd.getErr().println(cmd.getColorScheme().text(ufex.advice()));
            } else {
                cmd.getErr().println(cmd.getColorScheme().richStackTraceString(ex));
            }

            return cmd.getExitCodeExceptionMapper() != null
                    ? cmd.getExitCodeExceptionMapper().getExitCode(ex)
                    : cmd.getCommandSpec().exitCodeOnExecutionException();
        }
    }
}