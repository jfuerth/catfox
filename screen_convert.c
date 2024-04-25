#include <stdio.h>
#include "screen_convert.h"

/**
 * @brief write 16-bit little endian load address to stdout
 * 
 * @param loadaddr the value to write
 */
void start_chunk(u_int16_t loadaddr) {
    putchar(loadaddr & 0xff);
    putchar((loadaddr >> 8) & 0xff);
}

void end_chunk() {
    putchar(START_RUN);
    putchar(0);
}

void end_file() {
    putchar(0);
}

/*
 * converts screens exported in .prg format from https://petscii.krissz.hu/
 * into the crunched file format required by the interplat engine.
 */

int main(int argc, char const *argv[])
{
    const u_int16_t SCREEN_LOAD_ADDR = 0x4800;
    const u_int16_t COLOR_LOAD_ADDR = 0xd800;

    // skip to the start of screen data (at file location $2001)
    for (int i = 0; i < 0x2001; i++) {
        getchar();
    }
    
    // read and crunch the character data
    start_chunk(SCREEN_LOAD_ADDR);
    crunch_screen();
    end_chunk();

    // read and crunch the color data
    start_chunk(COLOR_LOAD_ADDR);
    crunch_screen();
    end_chunk();

    end_file();
    
    return 0;
}

// consumes the next 1000 bytes and outputs them in crunched form.
// special bytes:
// ff = run. followed by length, char
// fe = copy. followed by distance back, char count
void crunch_screen() {
    u_int8_t nextch = getchar();
    u_int16_t runlen = 0;
    for (int chars_read = 0; chars_read < 1000; ) {
        u_int8_t ch = nextch;
        runlen = 1;
        do {
            nextch = getchar();
            runlen++;
            chars_read++;
        } while (nextch == ch && chars_read < 1000 && runlen < 255);

        output_run(runlen - 1, ch, chars_read);
    }
    ungetc(nextch, stdin);
}

void output_run(u_int16_t runlen, u_int8_t ch, u_int16_t i) {
    fprintf(stderr, "Making run at i=%d: runlen=%d, ch=%d ('%c')\n", i, runlen, ch, ch);
    if (runlen == 0) {
        fprintf(stderr, "Error at i=%d: runlen=%d, ch=%d ('%c')\n", i, runlen, ch, ch);
    } else if (runlen <= 3 && ch < START_SIM) {
        // short run of non-special chars - just output literals
        for (int j = 0; j < runlen; j++) {
            putchar(ch);
        }
    } else {
        // run is long enough to be worth encoding, or has special chars - output run
        putchar(START_RUN);
        putchar(runlen);
        putchar(ch);
    }
}
