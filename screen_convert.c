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

void crunch_screen() {
    u_int8_t ch = 0;
    u_int16_t runlen = 0;
    u_int8_t prevch = 0;
    u_int8_t done = 0;
    u_int16_t i = 0;
    for (;;) {
        // cases to consider:
        //  * we are in a run (need to close it out)
        //  * we are not in a run (need to emit last char)
        ch = getchar();
        if (i == 0) {
            prevch = ch;
        }
        i++;
        runlen++;
        done = i == 40 * 25;

        if (ch == prevch && runlen < 255 && !done) {
            continue;
        }

        if (runlen == 0) {
            fprintf(stderr, "Error: runlen=%d, ch=%d, prevch=%d, i=%d\n", runlen, ch, prevch, i);
        } else if (runlen <= 3 && prevch < START_SIM) {
            // short run of non-special chars - just output literals
            for (int j = 0; j < runlen; j++) {
                putchar(prevch);
            }
        } else {
            // run is long enough to be worth encoding, or has special chars - output run
            putchar(START_RUN);
            putchar(runlen);
            putchar(prevch);
        }

        if (done) {
            break;
        }

        // reset for next char
        runlen = 0;
        prevch = ch;
    }
}
