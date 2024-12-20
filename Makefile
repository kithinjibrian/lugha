CC = gcc
CFLAGS = -std=c11 -ggdb -Wall -Werror -Wextra -D_POSIX_C_SOURCE=200809L -Wno-unused-function 
LDFLAGS = -lfl

SRC_DIR = src
INCLUDE_DIR = include
BUILD_DIR = build

# Source files
SRCS = $(shell find $(SRC_DIR) -type f -name '*.c')
OBJS = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRCS))

# Executable name
EXECUTABLE = lugha

.PHONY: all clean

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -I$(INCLUDE_DIR) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)

.PHONY: clean