#ifndef __RAM_H
#define __RAM_H

#include "common.h"

// #define EMU_RAM_SIZE (256 * 1024 * 1024UL)
#define EMU_RAM_SIZE (8 * 1024 * 1024 * 1024UL)

void init_ram(const char *img);
void ram_finish();
void* get_ram_start();
long get_ram_size();

void* get_img_start();
long get_img_size();

#ifdef WITH_DRAMSIM3
#include "axi4.h"

void dramsim3_finish();
void dramsim3_helper_rising(const struct axi_channel &axi);
void dramsim3_helper_falling(struct axi_channel &axi);
#endif

#endif
