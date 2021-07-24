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
