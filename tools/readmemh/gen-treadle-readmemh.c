#include <stdio.h>
#include <assert.h>
#include <stdint.h>

int main(int argc, char *argv[]) {
  assert(argc == 3);
  
  FILE *in = fopen(argv[1], "rb");
  assert(in != NULL);

  FILE *out = fopen(argv[2], "w");
  assert(out != NULL);

  int i;
  for (i = 0; i < 0x100000; i ++) {
    fprintf(out, "00\n");
  }
  
  uint8_t b;
  int ret;
  while ((ret = fread(&b, 1, 1, in)) != 0) {
    fprintf(out, "%1x%1x\n", b >> 4, b & 0xf);
  }

  fclose(in);
  fclose(out);

  return 0;
}
