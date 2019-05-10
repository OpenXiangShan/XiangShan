NAME = libdevice
INC_DIR += .
BUILD_DIR ?= ./build
OBJ_DIR ?= $(BUILD_DIR)/obj
BINARY ?= $(BUILD_DIR)/$(NAME).so

.DEFAULT_GOAL = app

# Compilation flags
CC = gcc
LD = gcc
INCLUDES  = $(addprefix -I, $(INC_DIR))
CFLAGS   += -O2 -MMD -Wall -Werror $(INCLUDES) -fomit-frame-pointer -fPIC

# Files to be compiled
SRCS = $(shell find . -name "*.c")
OBJS = $(SRCS:./%.c=$(OBJ_DIR)/%.o)

# Compilation patterns
$(OBJ_DIR)/%.o: %.c
	@echo + CC $<
	@mkdir -p $(dir $@)
	@$(CC) $(CFLAGS) -c -o $@ $<


# Depencies
-include $(OBJS:.o=.d)

.PHONY: app clean
app: $(BINARY)

$(BINARY): $(OBJS)
	@echo + LD $@
	@$(LD) -O2 -rdynamic -shared -fPIC -o $@ $^ -lSDL2 -lreadline -ldl

clean: 
	rm -rf $(BUILD_DIR)
