/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdint.h>

int main(int argc, char *argv[]) {
  assert(argc == 3);

  FILE *in = fopen(argv[1], "rb");
  assert(in != NULL);

  FILE *out = fopen(argv[2], "w");
  assert(out != NULL);

  char line[128];
  uint32_t addr;
  union {
    uint8_t _8[4];
    uint32_t _32;
  } data[4];
  while (fgets(line, 128, in) != NULL) {
    if (line[0] == '@') {
      sscanf(line + 1, "%x", &addr);
      assert(addr % 4 == 0);
      fprintf(out, "@%08x\n", addr / 4);
    }
    else {
      int ret = sscanf(line,
          "%hhx%hhx%hhx%hhx"
          "%hhx%hhx%hhx%hhx"
          "%hhx%hhx%hhx%hhx"
          "%hhx%hhx%hhx%hhx",
          &data[0]._8[0], &data[0]._8[1], &data[0]._8[2], &data[0]._8[3],
          &data[1]._8[0], &data[1]._8[1], &data[1]._8[2], &data[1]._8[3],
          &data[2]._8[0], &data[2]._8[1], &data[2]._8[2], &data[2]._8[3],
          &data[3]._8[0], &data[3]._8[1], &data[3]._8[2], &data[3]._8[3]);

      assert(ret == EOF || ret == 4 || ret == 8 || ret == 12 || ret == 16);

      if (ret == EOF) continue;

      if (ret >=  4) fprintf(out, "%08x ", data[0]._32);
      if (ret >=  8) fprintf(out, "%08x ", data[1]._32);
      if (ret >= 12) fprintf(out, "%08x ", data[2]._32);
      if (ret >= 16) fprintf(out, "%08x ", data[3]._32);
      fprintf(out, "\n");
    }
  }

  fclose(in);
  fclose(out);

  return 0;
}
