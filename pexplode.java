///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS com.fasterxml.jackson.core:jackson-databind:2.17.1
//DEPS info.picocli:picocli:4.7.6

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.core.exc.StreamReadException;
import com.fasterxml.jackson.databind.DatabindException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class pexplode {

    static final ObjectMapper objectMapper = JsonMapper.builder()
            .enable(StreamReadFeature.INCLUDE_SOURCE_IN_LOCATION)
            .findAndAddModules()
            .build();

    public static void main(String... args) throws StreamReadException, DatabindException, IOException {
        // read project file
        PetsciiProjectEnvelope outerProject = objectMapper.readValue(System.in, PetsciiProjectEnvelope.class);
        PetsciiProject innerProject = objectMapper.readValue(lzwDecompress(outerProject.data()), PetsciiProject.class);

        // output charsets
        for (CharsetBitmaps cs : innerProject.charsets()) {
            // TODO naming scheme for overriding load address
            // TODO command line arg for default charset address
            writePrgFile(
                    cs.name() + ".prg",
                    0x4000,
                    flatten(cs.bitmaps()));
        }

        // output screens
        for (Screen s : innerProject.screens()) {
            // TODO naming scheme for overriding load address
            // TODO command line arg for default screen address
            // TODO crunch the screen
            byte[] charData = flatten(s.charData());
            byte[] colourData = flatten(s.colorData());
            writeCrunchedFile(s.name(), Map.of(
                    0x4800, charData,
                    0xd800, cleanColourData(charData, colourData)));
        }

        // output sprites

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

    private static void writeCrunchedFile(String filename, Map<Integer, byte[]> segments) {
        System.err.printf("Writing %s%n", filename);
        try (OutputStream out = new FileOutputStream(filename)) {
            for (var segment : segments.entrySet()) {
                int addr = segment.getKey();
                byte[] rawBytes = segment.getValue();

                out.write((byte) (addr & 0xff));
                out.write((byte) (addr >> 8 & 0xff));
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
            if (length < 0 || length > 255) throw new AssertionError("Length %d out of range".formatted(length));
            if (start < 0) throw new AssertionError("Start %d out of range".formatted(start));
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
     * <pre>
     * In:   AAAABBCDDDAAAABBCDDD
     *        ^ startIdx = 1
     * 
     * Out:  ^ start = 0
     *           ^ length = 3
     * </pre>
     * 
     * Example 2: no overlap
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
     * repeated at startIdx, and its length. The similar sequence will start no
     * more than 255 characters before startIdx, and will have length no more than
     * 255. 
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
            out.write((byte) (loadAddress & 0xff));
            out.write((byte) (loadAddress >> 8 & 0xff));
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
}
