#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

const char *red = "\033[31m";
const char *green = "\033[32m";
const char *black = "\033[0m";

int read16() {
  int ch, val;
  ch = getc(stdin);
  val = ch;
  ch = getc(stdin);
  val |= (ch << 8);
  return val;
}

void dumpscr(int mem[], int ptr) {
  printf("%sPrinting screen at %x\n", black, ptr);
  for (int row = 0; row < 25; row++) {
    printf("r%2d (0x%04x): ", row, ptr);
    for (int col = 0; col < 40; col++) {
      if (mem[ptr] & 0x1000) {
        printf("%c[0m<", 0x1b);
      } else if (mem[ptr] & 0x2000) {
        printf("%c[0m>", 0x1b);
      } else {
        printf(" ");
      }
      char printchar = mem[ptr] & 0xff;
      if (printchar < ' ' || printchar > '~') printchar = '.';
      printf("%c[4%dm%c", 0x1b, (mem[ptr] & 0xf00) >> 8, printchar);
      ptr++;
    }
    printf("%c[0m\n", 0x1b);
  }
}

void dumpscr_linear(int mem[], int ptr) {
  printf("%c[0mlinear print so we can search for similarities\n", 0x1b);
  for (int i = 0; i < 1000; i++) {
    char printchar = mem[ptr] & 0xff;
    if (printchar < ' ' || printchar > '~') printchar = '.';
    printf("%c[4%dm%c", 0x1b, (mem[ptr] & 0xf00) >> 8, printchar);
    ptr++;
  }
  printf("%c[0m\n", 0x1b);
}

int main(int argc, char **argv) {
  int mem[65536];
  bzero(mem, sizeof(mem));
  int chunkbase, ptr;
  chunkbase = ptr = read16();
  
  // read start address
  printf("Starting to decode at %x\n", ptr);

  int ch;
  while (!feof(stdin)) {
    switch (ch) {
      case 0xff: {
          int len = getc(stdin);
          if (len == 0) {
            chunkbase = ptr = read16();
            printf("Starting new chunk at %x\n", ptr);
          } else {
            ch = getc(stdin);
            printf("%s  r%2d c%2d (0x%04x) run len=%3d char='%c' (%02x)\n",
                green, (ptr-chunkbase)/40, (ptr-chunkbase)%40, ptr, len, ch, ch);
            for (int i = 0; i < len; i++) {
              mem[ptr] = 0x200 | ch;
              if (i == 0) {
                mem[ptr] |= 0x1000; // start of run
              }
              if (i == len-1) {
                mem[ptr] |= 0x2000; // end of run
              }
              ptr++;
            }
          }
        }
        break;

      case 0xfe: {
          int len = getc(stdin);
          int offs = getc(stdin);
          printf("%s  r%2d c%2d (0x%04x) sim len=%3d offs=%3d: \"",
              red, (ptr-chunkbase)/40, (ptr-chunkbase)%40, ptr, len, offs);
          for (int i = 0; i < len; i++) {
            printf("%c", mem[ptr - offs]);
            mem[ptr] = 0x100 | (mem[ptr - offs] & 0xff);
            if (i == 0) {
              mem[ptr] |= 0x1000; // start of run
            }
            if (i == len-1) {
              mem[ptr] |= 0x2000; // end of run
            }
            ptr++;
          }
          printf("\"\n");
        }
        break;

      default:
        mem[ptr] = 0x300 | ch;
        ptr++;
        break;
    }
    ch = getc(stdin);
  }

  // make color mem readable
  for (ptr = 0xd800; ptr < 0xd800+(80*25); ptr++) {
    int val = mem[ptr] & 0xff;
    switch (val) {
      case 0x0: ch = 'k'; break;
      case 0x1: ch = 'w'; break;
      case 0x2: ch = 'r'; break;
      case 0x3: ch = 'c'; break;
      case 0x4: ch = 'p'; break;
      case 0x5: ch = 'g'; break;
      case 0x6: ch = 'b'; break;
      case 0x7: ch = 'y'; break;
      case 0x8: ch = 'o'; break;
      case 0x9: ch = 'w'; break;
      case 0xa: ch = 'R'; break;
      case 0xb: ch = '1'; break;
      case 0xc: ch = '2'; break;
      case 0xd: ch = 'G'; break;
      case 0xe: ch = 'B'; break;
      case 0xf: ch = '3'; break;
      default: ch = '?'; break;
    }
    mem[ptr] = (mem[ptr] & 0xffffff00) | ch;
  }

  if (argc == 2 && !strcmp(argv[1], "-l")) {
    dumpscr_linear(mem, 0x4800);
    dumpscr_linear(mem, 0xd800);
  } else {
    dumpscr(mem, 0x4800);
    dumpscr(mem, 0xd800);
  }
}

