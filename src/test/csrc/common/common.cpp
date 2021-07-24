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

#include <csignal>
#include "common.h"

int assert_count = 0;
static pthread_mutex_t assert_mutex;
int signal_num = 0;

void assert_init() {
  pthread_mutex_init(&assert_mutex, 0);
}

void assert_finish() {
  pthread_mutex_destroy(&assert_mutex);
}

extern "C" void xs_assert(long long line) {
  pthread_mutex_lock(&assert_mutex);
  if (assert_count >= 0) {
    printf("Assertion failed at line %lld.\n", line);
    assert_count++;
  }
  pthread_mutex_unlock(&assert_mutex);
}

void sig_handler(int signo) {
  if (signal_num != 0) 
    exit(0);
  signal_num = signo;
}
