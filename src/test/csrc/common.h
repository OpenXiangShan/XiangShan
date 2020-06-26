#ifndef __COMMON_H
#define __COMMON_H

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <stdint.h>
#include <assert.h>

#ifdef __cplusplus
# include <stdexcept>
# define print_and_die(s) throw std::runtime_error(s)
#else
# define print_and_die(s) do { fprintf(stderr,"%s\n",s); abort(); } while(0)
#endif

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#define eprintf(...) fprintf(stderr, ## __VA_ARGS__)

#define demand(cond,str,...) \
  do { if(!(cond)) { \
      char __str[256]; \
      snprintf(__str,256,"in %s, line %d: " str, \
               __FILE__,__LINE__,##__VA_ARGS__); \
      print_and_die(__str); \
    } } while(0)

// for debugging
int sc(unsigned int ncycle, int *ret_code);
int si(unsigned int ninstr, int *ret_code);
unsigned int read_reg(int reg_no);

// device
void init_device(void);
bool is_finished(void);
int get_exit_code(void);

// log
enum {
  LOG_ALL = 0,
  LOG_DEBUG,
  LOG_INFO,
  LOG_WARN,
  LOG_ERROR,
  LOG_OFF
};

uint64_t getLogLevel(const char * str);

void app_error(const char *fmt, ...);

int monitor(void);
#endif // __COMMON_H
