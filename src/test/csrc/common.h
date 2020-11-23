#ifndef __COMMON_H
#define __COMMON_H

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <stdint.h>
#include <assert.h>

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#define eprintf(...) fprintf(stdout, ## __VA_ARGS__)

#ifdef WITH_DRAMSIM3
#include "cosimulation.h"
#endif

#endif // __COMMON_H
