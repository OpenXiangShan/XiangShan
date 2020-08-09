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
