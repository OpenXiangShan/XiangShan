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
#include <string.h>

char outname [4][4096];

int main(int argc, char *argv[]) {
  assert(argc == 2);

  FILE *in = fopen(argv[1], "rb");
  assert(in != NULL);

  strcat(stpcpy(outname[0], argv[1]), "_0");
  strcat(stpcpy(outname[1], argv[1]), "_1");
  strcat(stpcpy(outname[2], argv[1]), "_2");
  strcat(stpcpy(outname[3], argv[1]), "_3");

  FILE *out[4];
  out[0] = fopen(outname[0], "w");
  out[1] = fopen(outname[1], "w");
  out[2] = fopen(outname[2], "w");
  out[3] = fopen(outname[3], "w");
  assert(out[0] != NULL && out[1] != NULL && out[2] != NULL && out[3] != NULL);

  char line[128];
  int idx = 0;
  while (fgets(line, 128, in) != NULL) {
    if (line[0] == '@') {
      uint32_t addr;
      sscanf(line + 1, "%x", &addr);
      assert(addr % 4 == 0);
      fprintf(out[0], "\n@%08x\n", addr / 4);
      fprintf(out[1], "\n@%08x\n", addr / 4);
      fprintf(out[2], "\n@%08x\n", addr / 4);
      fprintf(out[3], "\n@%08x\n", addr / 4);
      idx = 0;
    }
    else {
      // remove white spaces at the end
      char *p = line + strlen(line) - 1;
      while (p >= line && (*p == ' ' || *p == '\n' || *p == '\r')) p --;
      p[1] = '\0';

      p = line;
      char *byte;
      while ((byte = strsep(&p, " "))) {
        fprintf(out[idx % 4], "%s ", byte);
        idx ++;
      }

      if ((idx >> 2) % 16 == 0) {
        fprintf(out[0], "\n");
        fprintf(out[1], "\n");
        fprintf(out[2], "\n");
        fprintf(out[3], "\n");
      }
    }
  }

  fclose(in);
  fclose(out[0]);
  fclose(out[1]);
  fclose(out[2]);
  fclose(out[3]);

  return 0;
}
