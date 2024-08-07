///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS com.fasterxml.jackson.core:jackson-databind:2.17.1
//DEPS info.picocli:picocli:4.7.6

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
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
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ParseResult;
import picocli.CommandLine.Spec;

public class pexplode implements Runnable {

    static final ObjectMapper objectMapper = JsonMapper.builder()
            .enable(StreamReadFeature.INCLUDE_SOURCE_IN_LOCATION)
            .findAndAddModules()
            .build();

    public static void main(String... args) throws StreamReadException, DatabindException, IOException {
        int result = new CommandLine(new pexplode())
                .setExecutionExceptionHandler(new UserFacingExceptionHandler())
                .execute(args);
        System.exit(result);
    }

    public enum AssetType {
        SPRITES, CHARSETS, SCREENS
    }

    static class SpriteInfos {
        private final Map<String, SpriteInfo> uidMap;

        public SpriteInfos(Map<String, pexplode.SpriteInfo> uidMap) {
            this.uidMap = uidMap;
        }

        public SpriteInfo findByUid(String uid) {
            return uidMap.get(uid);
        }

        public SpriteInfo findByName(String name) {
            return uidMap.values().stream()
                    .filter(si -> si.name().equals(name))
                    .findAny()
                    .orElseThrow(() -> new IllegalArgumentException("Can't find sprite frame named \"" + name + "\""));
        }
    }

    record SpriteInfo(String name, int address) {
        /** Returns the VIC-II sprite number for this address. */
        int spriteNum() {
            return (address % 0x4000) / 64;
        }
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

    static class Symbols {
        Map<String, Integer> symbols = new LinkedHashMap<>();

        /**
         * Resolves the given string, which could be a named symbol, a literal address, or
         * null (resolves to 0).
         */
        public Integer resolve(String nameOrAddress) {
            if (nameOrAddress == null) {
                return 0;
            }
            Integer addr = symbols.get(nameOrAddress);
            if (addr != null) {
                return addr;
            }
            return parseAsmAddr(nameOrAddress);
        }

        /** Adds the symbols in the given file to this set of symbols. */
        public void load(File symbolsFile) {
            try (var r = new BufferedReader(new FileReader(symbolsFile))) {
                r.lines()
                        .map(l -> removeComments(l).trim())
                        .filter(l -> !l.isEmpty())
                        .peek(l -> debugPrintf("trimmed-symbols-line: %s%n", l))
                        .forEach(l -> {
                            String[] parts = l.split("=", 2);
                            String sym = parts[0];
                            String addrExpr = parts[1];
                            symbols.put(sym, parseAsmAddr(addrExpr));
                        });
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        private static String removeComments(String line) {
            int commentStart = line.indexOf(';');
            if (commentStart >= 0) {
                return line.substring(0, commentStart);
            }
            return line;
        }

        private static int parseAsmAddr(String addrExpr) {
            String trimmed = addrExpr.trim();
            try {
                return switch (trimmed.charAt(0)) {
                    case '$' -> Integer.parseInt(trimmed.substring(1), 16);
                    case '%' -> Integer.parseInt(trimmed.substring(1), 2);
                    default -> Integer.parseInt(trimmed.substring(1));
                };
            } catch (Exception e) {
                throw new IllegalArgumentException("Bad address \"%s\"".formatted(addrExpr));
            }
        }
    }

    @Spec CommandSpec spec;

    @Option(names = "--debug", description = "Print debug info to stderr")
    static boolean debug;

    @Option(names = "--process", description = "Which asset types to process", split = ",", defaultValue = "SPRITES,CHARSETS,SCREENS")
    List<AssetType> process;

    @Option(names = "--first-sprite-addr", required = true, description = "Load address (literal or symbol) for first sprite")
    String firstSpriteAddr;

    @Option(names = {
            "--select-charsets" }, defaultValue = "*", description = "Names of charsets to process. Glob matching is supported.")
    Set<String> charsetGlobs;

    @CommandLine.Option(names = { "-l", "--load-addr" }, description = """
            Set or override load address (symbol or literal) for an asset.
            The asset name is a glob, matched case-insensitively by name.
            This option overrides any load address specified in a part's name.
            For screens, this sets the load address of the character matrix.
            Use --color-load-address to set the load address of the colour matrix.
            """)
    Map<String, String> loadAddresses = new LinkedHashMap<>();

    @CommandLine.Option(names = { "-c", "--color-load-addr", "--colour-load-addr" }, description = """
            Set or override load address (symbol or literal) for a screen's colour matrix.
            All colour matrix load addresses otherwise default to $d800.
            """)
    Map<String, String> colorLoadAddresses = new LinkedHashMap<>();

    @Option(names = "--mobtab-addr", description = "Load address (symbol or literal) for mob table segment saved with screens")
    String mobTableAddr;

    @Option(names = "--symbols-file", description = "File with name=address mappings for C64 addresses")
    Collection<File> symbolsFiles = new ArrayList<>();

    @Option(names = "--mob-behaviours-file", description = "JSON file specifying mob behaviours for sprites on screen")
    File mobBehavioursFile;

    @Parameters(description = "PETSCII Editor (.pe) file to read")
    Path inputFile;

    Symbols symbols = new Symbols();

    @Override
    public void run() {
        downcaseKeys(loadAddresses);
        downcaseKeys(colorLoadAddresses);

        for (File symbolsFile : symbolsFiles) {
            debugPrintf("Loading symbols from %s...%n", symbolsFile);
            symbols.load(symbolsFile);
        }

        PetsciiProject peProject;
        try {
            PetsciiProjectEnvelope envelope = objectMapper.readValue(inputFile.toFile(),
                    PetsciiProjectEnvelope.class);
            String innerJson = lzwDecompress(envelope.data());
            debugPrintf("Decompressed JSON of .pe file: %s%n", innerJson);
            peProject = objectMapper.readValue(innerJson, PetsciiProject.class);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        SpriteInfos spriteInfos = calcSpriteInfo(peProject, symbols.resolve(firstSpriteAddr));

        if (process.contains(AssetType.SPRITES)) {
            processSprites(peProject, spriteInfos);
        }

        if (process.contains(AssetType.CHARSETS)) {
            for (CharsetBitmaps cs : peProject.charsets()) {
                if (matchesAnyGlob(cs.name(), charsetGlobs))
                    writePrgFile(
                            cs.name() + ".prg",
                            determineLoadAddress(cs.name()),
                            flatten(cs.bitmaps()));
            }
        }

        if (process.contains(AssetType.SCREENS)) {
            if (mobBehavioursFile == null) {
                throw new ParameterException(
                    spec.commandLine(),
                     "--mob-behaviours-file is required when processing screens");
            }
            MobBehaviours mobBehaviours;
            try {
                mobBehaviours = objectMapper.readValue(mobBehavioursFile, MobBehaviours.class);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }

            for (Screen s : peProject.screens()) {
                byte[] charData = flatten(s.charData());
                byte[] colourData = flatten(s.colorData());
                byte[] mobTable = createMobTable(s.sprites, spriteInfos, mobBehaviours);
                writeCrunchedFile(s.name(), Map.of(
                        determineLoadAddress(s.name), charData,
                        determineColorLoadAddress(s.name), cleanColourData(charData, colourData),
                        symbols.resolve(mobTableAddr), mobTable));
            }
        }
    }

    private SpriteInfos calcSpriteInfo(PetsciiProject peProject, int firstSpriteAddr) {
        Map<String, SpriteInfo> result = new LinkedHashMap<>();
        int nextSpriteAddr = firstSpriteAddr;
        for (SpriteSet ss : peProject.spriteSets()) {
            for (int i = 0; i < ss.sprites().size(); i++) {
                Sprite s = ss.sprites().get(i);
                int numberInUi = i + 1;
                String name = ss.name() + "_" + numberInUi;
                result.put(s.uid(), new SpriteInfo(name, nextSpriteAddr));
                nextSpriteAddr += 64;
            }
        }
        return new SpriteInfos(result);
    }

    /**
     * Outputs sprites.prg and sprites_nums.s.
     * <p>
     * at this point, we want all sprites mashed consecutively into one file,
     * and a companion include file that defines sprite numbers, which is
     * {@code [address within vic bank] / 64}.
     * <p>
     * Sometimes you'd want something else, for example if you need to load some
     * sprites separately for different screens or phases of the game.
     * Parameterizing all of this is possible but maybe not worth it at this stage
     * since the eventual requirements aren't known.
     * 
     * @param spriteInfos maps each Sprite::uid to its name and expected address.
     */
    private void processSprites(PetsciiProject peProject, SpriteInfos spriteInfos) {
        final String spriteSetName = "sprites";
        final File spritenumsFile = new File(spriteSetName + "_nums.s");
        final File spriteDataFile = new File(spriteSetName + ".prg");

        try (PrintWriter spriteNumsOut = new PrintWriter(spritenumsFile);
                OutputStream dataOut = new FileOutputStream(spriteDataFile)) {

            SpriteInfo lastSprite = null;
            for (SpriteSet ss : peProject.spriteSets()) {
                for (int i = 0; i < ss.sprites().size(); i++) {
                    Sprite s = ss.sprites().get(i);
                    SpriteInfo si = spriteInfos.findByUid(s.uid());

                    if (lastSprite == null) {

                        // emit load address for .prg file
                        writeWord(dataOut, si.address());

                        spriteNumsOut.printf("%s_first=%d%n", spriteSetName, si.spriteNum());
                    }

                    spriteNumsOut.printf("%s=%d ; addr=%04x%n", si.name(), si.spriteNum(), si.address());

                    for (int[] spriteBits : s.bitmapData) {
                        byte b = condenseBits(spriteBits);
                        dataOut.write(b);
                    }
                    dataOut.write((byte) 0); // pad to 64 bytes
                    lastSprite = si;
                }
            }
            spriteNumsOut.printf("%s_last=%d%n", spriteSetName, lastSprite.spriteNum() + 1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static record MobTableEntry(
            // mobxl=0
            // mobxh=1
            float x,
            // mobdxl=2
            // mobdxh=3
            float dx,
            // mobyl=4
            // mobyh=5
            float y,
            // mobdyl=6
            // mobdyh=7
            float dy,
            // mobcolr=8 ; bit 7 set: disabled
            // ; bit 6 set: x-mirrored
            int color,
            boolean disabled,
            boolean xMirror,
            // mobimg=9
            int imageIndex,
            // mobalist=10 ; +11
            int alistAddr,
            // mobaframe=12
            int animFrame,
            // mobattl=13 ; countdown current frame
            int animTtl,
            // mobact=14 ; +15
            int actionAddr) {

        /** Size of a mobtab in (C64 memory) bytes. */
        public static final int SIZE = 16;

        /** Offset of screen coordinate vs mob coordinate. */
        private static int spriteXOffset = 24;

        /** Offset of screen coordinate vs mob coordinate. */
        private static int spriteYOffset = 29;

        public MobTableEntry(
                float screenX,
                float dx,
                float screenY,
                float dy,
                int color,
                int vicSpriteNum,
                int alistAddr,
                int actionAddr) {
            this(
                    (screenX - spriteXOffset) / 8f,
                    dx,
                    (screenY - spriteYOffset) / 8f,
                    dy,
                    color,
                    false,
                    false, // mob action will set this (usually based on dx)
                    vicSpriteNum, // alist will override this
                    alistAddr,
                    0,
                    0,
                    actionAddr);
        }

        public void write(byte[] dst, int offset) {
            final int origOffset = offset;
            write88FixedPoint(dst, offset, x);
            offset += 2;
            write88FixedPoint(dst, offset, dx);
            offset += 2;
            write88FixedPoint(dst, offset, y);
            offset += 2;
            write88FixedPoint(dst, offset, dy);
            offset += 2;
            dst[offset++] = (byte) (color
                    | (disabled ? 0b10000000 : 0)
                    | (xMirror ? 0b01000000 : 0));
            dst[offset++] = (byte) imageIndex;
            write16(dst, offset, alistAddr);
            offset += 2;
            dst[offset++] = (byte) animFrame;
            dst[offset++] = (byte) animTtl;
            write16(dst, offset, actionAddr);
            offset += 2;

            if (offset - origOffset != SIZE) {
                throw new AssertionError("Made " + offset + " bytes");
            }
        }

        private static void write16(byte[] dst, int offset, int val) {
            dst[offset] = (byte) (val & 0xff);
            dst[offset + 1] = (byte) (val >> 8 & 0xff);
        }

        private static void write88FixedPoint(byte[] dst, int offset, float val) {
            int intPart = (int) val;
            int fraction = (int) ((val - intPart) * 256);
            dst[offset] = (byte) (fraction & 0xff);
            dst[offset + 1] = (byte) (intPart & 0xff);
        }
    }

    /**
     * JSON file providing mobtab settings that aren't available in the SpriteOnScreen from the .pe
     * file.
     */
    static record MobBehaviours(
            List<MobBehaviours.Entry> behaviours) {

        public MobBehaviours.MobTableSpec findMobTableSpec(SpriteOnScreen s, SpriteInfos spriteInfos) {
            pexplode.SpriteInfo si = spriteInfos.findByUid(s.uid());
            debugPrintf("Looking up behaviours for %s...%n", si);
            for (Entry ent : behaviours) {
                if (ent.matches(s, spriteInfos)) {
                    debugPrintf("Found %s%n", ent);
                    return ent.mobtab();
                }
            }
            throw new IllegalArgumentException("No mobmap entry matches " + s + "(" + si + ")");
        }

        static record Entry(
                String match,
                MobTableSpec mobtab) {

            /** Tests if this mobmap entry matches the given sprite by evaluating the match expression. */
            boolean matches(SpriteOnScreen sos, SpriteInfos spriteInfos) {
                String[] parts = match.split("=");
                if (parts.length != 2) {
                    throw new IllegalArgumentException("Bad match expression \"" + match + "\"");
                }
                String field = parts[0];
                String value = parts[1];
                return switch (field) {
                    case "sprite.name" -> value.equals(sos.name(spriteInfos));
                    default -> throw new IllegalArgumentException("Unsupported field \"" + field + "\"");
                };
            }
        }

        static record MobTableSpec(
                float dx,
                float dy,
                String alist,
                String action) {
        }
    }

    private byte[] createMobTable(
            List<SpriteOnScreen> sprites,
            SpriteInfos spriteInfos,
            MobBehaviours mobmap) {

        byte[] mobtab = new byte[MobTableEntry.SIZE * sprites.size() + 2];
        int offset = 0;
        for (SpriteOnScreen s : sprites) {
            MobBehaviours.MobTableSpec spec = mobmap.findMobTableSpec(s, spriteInfos);
            MobTableEntry me = new MobTableEntry(
                    s.x(), spec.dx(),
                    s.y(), spec.dy(),
                    s.color(),
                    spriteInfos.findByUid(s.uid()).spriteNum(),
                    symbols.resolve(spec.alist()),
                    symbols.resolve(spec.action()));
            me.write(mobtab, offset);
            offset += MobTableEntry.SIZE;
        }

        // mark end of table: sentinel value x-coord of 8080
        // actually any second byte here > 40 is off screen, so there are
        // many usable sentinel values that could mean other stuff
        mobtab[offset++] = (byte) 0x80;
        mobtab[offset++] = (byte) 0x80;

        return mobtab;
    }

    /**
     * Takes an 8-element array of 0s and 1s (MSB first) and returns the
     * corresponding byte.
     */
    private static byte condenseBits(int[] spriteBits) {
        if (spriteBits.length != 8) {
            throw new IllegalArgumentException("spriteBits.length is " + spriteBits.length + "; expected 8");
        }
        int b = 0;
        for (int i = 0; i < 8; i++) {
            int v = spriteBits[i];
            if (v != 0 && v != 1) {
                throw new IllegalArgumentException(
                        "spriteBits[" + i + "] is " + spriteBits[i] + "; expected 1 or 0");
            }
            b = b << 1;
            b |= v;
        }
        return (byte) (b & 0xff);
    }

    private static void downcaseKeys(Map<String, ?> map) {
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

    private static final Pattern PART_NAME_LOAD_ADDRESS_PATTERN = Pattern.compile("(\\$[0-9a-fA-F]+)");

    private int determineLoadAddress(String name) {
        return determineLoadAddress(name, loadAddresses, null);
    }

    private int determineColorLoadAddress(String name) {
        return determineLoadAddress(name, colorLoadAddresses, "$d800");
    }

    /**
     * Finds the load address for the named segment (screen or charset). All
     * address values are resolved through the {@link #symbols} resolver.
     */
    private int determineLoadAddress(String name, Map<String, String> nameMap, String defaultValue) {
        Matcher partNameMatcher = PART_NAME_LOAD_ADDRESS_PATTERN.matcher(name);
        String override = getByGlob(nameMap, name.toLowerCase());
        if (override != null) {
            return symbols.resolve(override);
        } else if (partNameMatcher.find()) {
            return symbols.resolve(partNameMatcher.group(1));
        } else if (defaultValue != null) {
            return symbols.resolve(defaultValue);
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

    private void writeCrunchedFile(String filename, Map<Integer, byte[]> segments) {
        infoPrintf("Writing %s%n", filename);
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

                debugPrintf("  $%x - %d -> %d (%.2f%%)%n",
                        addr, rawBytes.length, crunchedBytes.length,
                        ((float) crunchedBytes.length) / ((float) rawBytes.length) * 100f);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    byte[] crunch(byte[] rawBytes) {
        debugPrintf("Crunching %d bytes%n", rawBytes.length);
        ArrayList<Byte> compressed = new ArrayList<>();
        for (int i = 0; i < rawBytes.length;) {
            int runlen = findRun(rawBytes, i);
            Similarity repeat = findSim(rawBytes, i);

            if (repeat.length() > 3 && repeat.length() > runlen) {
                debugPrintf("Using sim length %d start %d: %s%n", repeat.length(), repeat.start(),
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
            } else if ((rawBytes[i] & 0xff) >= 0xfe) {
                compressed.add((byte) 0xff);
                compressed.add((byte) 1);
                compressed.add((byte) rawBytes[i]);
                i += 1;
            } else {
                compressed.add((byte) rawBytes[i]);
                i += 1;
            }
        }
        byte result[] = new byte[compressed.size()];
        for (int i = 0; i < compressed.size(); i++) {
            result[i] = (byte) (compressed.get(i) & 0xff);
        }
        verifyCrunchedData(rawBytes, result);
        return result;
    }

    private static void verifyCrunchedData(
            byte[] original,
            byte[] crunched) {
        byte[] decrunched = new byte[original.length];
        try {
            for (int i = 0, j = 0; i < crunched.length; i++) {
                switch ((int) crunched[i] & 0xff) {
                    case 0xff -> {
                        int len = crunched[++i] & 0xff;
                        byte ch = crunched[++i];
                        for (int k = 0; k < len; k++) {
                            decrunched[j + k] = ch;
                        }
                        j += len;
                    }
                    case 0xfe -> {
                        int len = crunched[++i] & 0xff;
                        int offset = crunched[++i] & 0xff;
                        for (int k = 0; k < len; k++) {
                            decrunched[j + k] = decrunched[j - offset + k];
                        }
                        j += len;
                    }
                    default -> {
                        decrunched[j] = crunched[i];
                        j++;
                    }
                }
            }
        } catch (IndexOutOfBoundsException e) {
            System.err.println("Decrunched data too big: " + e.getMessage());
        }
        for (int i = 0; i < original.length; i++) {
            if (decrunched[i] != original[i]) {

                throw new AssertionError("""
                        decrunched[$%x] = $%x, original[$%x] = $%x%n
                        Original:%n
                        %s%n
                        Crunched:%n
                        %s%n
                        Decrunched:%n
                        %s%n
                        """.formatted(i, decrunched[i], i, original[i],
                        hexdump(original), hexdump(crunched), hexdump(decrunched)));
            }
        }
    }

    private static String hexdump(byte[] data) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < data.length; i++) {
            if (i % 16 == 0) {
                sb.append("%05x ".formatted(i));
            }
            sb.append("%02x".formatted(data[i] & 0xff));
            if (i % 16 == 15) {
                sb.append(System.lineSeparator());
            } else {
                sb.append(" ");
            }
        }
        return sb.toString();
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
    private static Similarity findSim(byte[] rawBytes, final int startIdx) {
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

    void writePrgFile(String filename, int loadAddress, byte[] bytes) {
        infoPrintf("Writing %s ($%x)%n", filename, loadAddress);
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
            List<SpriteOnScreen> sprites,
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

    static record SpriteOnScreen(
            int setId,
            String uid,
            int x,
            int y,
            int color,
            boolean expandX,
            boolean expandY,
            String priority) {

        /** Looks up name frame name of this sprite-on-screen. */
        public String name(SpriteInfos spriteInfos) {
            return spriteInfos.findByUid(uid).name();
        }
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

    private static void debugPrintln(String msg) {
        if (debug) {
            System.err.println(msg);
        }
    }

    private static void debugPrintf(String format, Object... args) {
        if (debug) {
            System.err.printf(format, args);
        }
    }

    private static void infoPrintln(String msg) {
        System.err.println(msg);
    }

    private static void infoPrintf(String format, Object... args) {
        System.err.printf(format, args);
    }
}