#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void uart_putchar(char c)
{
  printf("%c", c);
  fflush(stdout);
}