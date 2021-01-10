#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>
#include<sys/stat.h>
#include<fcntl.h>

int tryLock(char * file){
    return open(file, O_CREAT | O_EXCL);
}

int main(int argc, char* argv[]){
    int fd;
	if(argc < 2){
	    printf("arguments are not right!\n");
		exit(-1);
	}

    do{
        fd = tryLock(argv[1]);
        if(fd > 0) break;
		printf("there is a job running, waiting ...\n");
		sleep(10);
	} while(1);

    return 0;
}

