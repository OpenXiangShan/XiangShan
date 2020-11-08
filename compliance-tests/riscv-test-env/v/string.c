#include <string.h>
#include <stdint.h>
#include <ctype.h>

void* memcpy(void* dest, const void* src, size_t len)
{
  if ((((uintptr_t)dest | (uintptr_t)src | len) & (sizeof(uintptr_t)-1)) == 0) {
    const uintptr_t* s = src;
    uintptr_t *d = dest;
    while (d < (uintptr_t*)(dest + len))
      *d++ = *s++;
  } else {
    const char* s = src;
    char *d = dest;
    while (d < (char*)(dest + len))
      *d++ = *s++;
  }
  return dest;
}

void* memset(void* dest, int byte, size_t len)
{
  if ((((uintptr_t)dest | len) & (sizeof(uintptr_t)-1)) == 0) {
    uintptr_t word = byte & 0xFF;
    word |= word << 8;
    word |= word << 16;
    word |= word << 16 << 16;

    uintptr_t *d = dest;
    while (d < (uintptr_t*)(dest + len))
      *d++ = word;
  } else {
    char *d = dest;
    while (d < (char*)(dest + len))
      *d++ = byte;
  }
  return dest;
}

size_t strlen(const char *s)
{
  const char *p = s;
  while (*p)
    p++;
  return p - s;
}

int strcmp(const char* s1, const char* s2)
{
  unsigned char c1, c2;

  do {
    c1 = *s1++;
    c2 = *s2++;
  } while (c1 != 0 && c1 == c2);

  return c1 - c2;
}

int memcmp(const void* s1, const void* s2, size_t n)
{
  if ((((uintptr_t)s1 | (uintptr_t)s2) & (sizeof(uintptr_t)-1)) == 0) {
    const uintptr_t* u1 = s1;
    const uintptr_t* u2 = s2;
    const uintptr_t* end = u1 + (n / sizeof(uintptr_t));
    while (u1 < end) {
      if (*u1 != *u2)
        break;
      u1++;
      u2++;
    }
    n -= (const void*)u1 - s1;
    s1 = u1;
    s2 = u2;
  }

  while (n--) {
    unsigned char c1 = *(const unsigned char*)s1++;
    unsigned char c2 = *(const unsigned char*)s2++;
    if (c1 != c2)
      return c1 - c2;
  }

  return 0;
}

char* strcpy(char* dest, const char* src)
{
  char* d = dest;
  while ((*d++ = *src++))
    ;
  return dest;
}

long atol(const char* str)
{
  long res = 0;
  int sign = 0;

  while (*str == ' ')
    str++;

  if (*str == '-' || *str == '+') {
    sign = *str == '-';
    str++;
  }

  while (*str) {
    res *= 10;
    res += *str++ - '0';
  }

  return sign ? -res : res;
}
