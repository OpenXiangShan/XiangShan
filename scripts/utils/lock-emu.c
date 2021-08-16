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

#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<string.h>

#define BUF_SIZE 32

int tryLock(char * file){
    return open(file, O_CREAT | O_EXCL | O_WRONLY, 0666);
}

int main(int argc, char* argv[]){
    int fd;
	char user[BUF_SIZE];
	if(argc < 2){
	    printf("arguments are not right!\n");
		exit(-1);
	}

    do{
        fd = tryLock(argv[1]);
        if(fd > 0){
            getlogin_r(user, BUF_SIZE);
            int len = strlen(user);
            user[len] = '\0';
            write(fd, user, len+1);
            break;
        } else {
            // someone is holding the lock...
            fd = open(argv[1], O_RDONLY);
            if(fd > 0){
                read(fd, user, BUF_SIZE);
                printf("%s is holding the lock, waiting ...\n", user);
            }
        }
        sleep(10);
    } while(1);

    return 0;
}

