#include <cstdio>
#include <cstring>
#include "common.h"

uint64_t getLogLevel(const char * str) {
  if(!strcmp("ALL", str)){
    return LOG_ALL;
  } else if(!strcmp("DEBUG", str)){
    return LOG_DEBUG;
  } else if(!strcmp("INFO", str)){
    return LOG_INFO;
  } else if(!strcmp("WARN", str)){
    return LOG_WARN;
  } else if(!strcmp("ERROR", str)){
    return LOG_ERROR;
  } else if(!strcmp("OFF", str)){
    return LOG_OFF;
  } else {
    printf("Unknown verbosity level!\n");
    exit(-1);
  }
}
