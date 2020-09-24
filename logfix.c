#include <stdio.h>

int main() {
    char c = getchar(), flag = 0;
    while (c != EOF) {
        if (c != '\n' || flag == 1) {
            printf("%c", c);
        }
        flag = (c == '\n') ? 1 - flag : 0;
        c = getchar();
    }
    return 0;
}
