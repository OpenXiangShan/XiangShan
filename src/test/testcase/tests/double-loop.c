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

// #include <am.h>
// #include <klib.h>

# define OutLoopNum 20
# define InnerLoopNum 20

int array[OutLoopNum][InnerLoopNum];

void double_loop() {
    for (int i = 0; i < OutLoopNum; i++) {
        for (int j = 1; j < InnerLoopNum; j++) {
            array[i][j] = i + j;
        }
    }
}

int main () {
    double_loop();
    return 0;
}
