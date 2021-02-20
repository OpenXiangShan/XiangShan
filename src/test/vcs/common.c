#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void xs_assert(long long line) {
  printf("Assertion failed at line %lld.\n", line);
}

