#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_MAX 1000

int getaddr(char *label, char *line) {
  char scanbuf[5];
  for (int i = 0; i < 4; i++) {
    scanbuf[i] = line[i+5];
  }
  scanbuf[4] = '\0';
  char *endptr;
  //printf("    [%20s][%s]--scanning line for address: [%s]\n", label, scanbuf, line);
  long addr = strtol(scanbuf, &endptr, 16);
  if (endptr == scanbuf+4) {
    return addr;
  }
  return -1;
}

int main(int argc, char **argv) {
  char line[LINE_MAX];

  while (fgets(line, LINE_MAX, stdin) != NULL) {
    int len = strlen(line);
    if (len > 23 && line[23] >= 'a' && line[23] <= 'z') {
      // found label. extract it.
      char label[LINE_MAX];
      sscanf(line+23, "%s", label);

      // not actually a label if it has "=" in it
      if (strstr(line, "=") != NULL) {
        continue;
      }

      // find closest line (starting with this one) that has an address. That's the label's address.
      int address;
      for (;;) {
        address = getaddr(label, line);
        if (address != -1) break;
        if (fgets(line, LINE_MAX, stdin) == NULL) {
          fprintf(stderr, "got EOF looking for address of label %s\n", label);
          return 1;
        }
      }

      // emit vice label
      printf("add_label %x .%s\n", address, label);
    }
  }
}

