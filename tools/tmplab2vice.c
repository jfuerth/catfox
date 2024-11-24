#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_MAX 1000

static size_t safe_strcpy(char *dst, const char *source, size_t size) {
  size_t slen = strlen(source);
  if (slen > size - 1) {
    slen = size - 1;
  }
  for (int i = 0; i < slen; i++) {
    dst[i] = source[i];
  }
  dst[slen] = '\0';
  return slen;
}

int getaddr(char *label, char *line) {
  char scanbuf[5];
  for (int i = 0; i < 4; i++) {
    scanbuf[i] = line[i+5];
  }
  scanbuf[4] = '\0';
  char *endptr;
  fprintf(stderr, "    [%20s][%s]--scanning line for address: [%s]\n", label, scanbuf, line);
  long addr = strtol(scanbuf, &endptr, 16);
  if (endptr == scanbuf+4) {
    return addr;
  }
  return -1;
}

void push_path(char *path, const char *segment) {
  char *clean_segment = strndup(segment, LINE_MAX);

  // change non-alphanumerics to _
  for (int i = 0; clean_segment[i] != '\0' && i < LINE_MAX; i++) {
    if ( (clean_segment[i] >= 'a' && clean_segment[i] <= 'z') ||
         (clean_segment[i] >= '0' && clean_segment[i] <= '9')) {
      // good
    } else {
      clean_segment[i] = '_';
    }
  }

  strncat(path, ".", LINE_MAX);
  strncat(path, clean_segment, LINE_MAX);

  fprintf(stderr, "Pushed %s to path: %s\n", segment, path);

  free(clean_segment);
}

void pop_path(char *path) {
  for (int i = strlen(path); i >= 0; i--) {
    if (path[i] == '.') {
      path[i] = '\0';
      break;
    }
  }
  fprintf(stderr, "Popped path: %s\n", path);
}

int update_path(const char *line, char *blockpath, const char *lastlabel) {
  if (strstr(line, ".block")) {
    push_path(blockpath, lastlabel);
    return 1;
  }

  if (strstr(line, ".bend")) {
    pop_path(blockpath);
    return -1;
  }

  return 0;
}

int main(int argc, char **argv) {
  char line[LINE_MAX];

  // track the .block/.bend nesting structure
  char lastlabel[LINE_MAX];
  lastlabel[0] = '\0';
  char blockpath[LINE_MAX];
  blockpath[0] = '\0';

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

      // also skip variable and macro directives because they have no single value
      if (strstr(line, ".var") != NULL) {
        continue;
      }
      if (strstr(line, ".macro") != NULL) {
        continue;
      }
      if (strstr(line, ".segment") != NULL) {
        continue;
      }

      safe_strcpy(lastlabel, label, LINE_MAX);

      // remember original path so we can print the label correctly
      // if we encounter a .block or .bend while looking for the address
      char oldpath[LINE_MAX];
      safe_strcpy(oldpath, blockpath, LINE_MAX);

      // find closest line (starting with this one) that has an address. That's the label's address.
      int address;
      for (;;) {
        address = getaddr(label, line);
        if (address != -1) break;
        if (fgets(line, LINE_MAX, stdin) == NULL) {
          fprintf(stderr, "got EOF looking for address of label \"%s\"\n", label);
          return 1;
        }
        update_path(line, blockpath, lastlabel);
      }

      // emit vice label
      printf("add_label %x %s.%s\n", address, oldpath, label);
    } else {
      update_path(line, blockpath, lastlabel);
    }
  }
}

