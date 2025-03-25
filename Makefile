CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -D_POSIX_C_SOURCE=200809L
LDFLAGS = 

# Main executable
TRANSLATOR = holywc

# Source files
SRCS = holywc.c

# Object files
OBJS = $(SRCS:.c=.o)

# Default target
all: $(TRANSLATOR)

# Linking the translator
$(TRANSLATOR): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Compiling source files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(TRANSLATOR) $(OBJS) *.wat *.wasm

# Translation targets
translate: $(TRANSLATOR)
	./$(TRANSLATOR) example.hw example.wat

wasm: translate
	wat2wasm example.wat -o example.wasm

run: wasm
	node run.js

# Install dependencies (for systems that don't have wat2wasm)
install-deps:
	@echo "Installing WABT (WebAssembly Binary Toolkit)..."
	@if [ -x "$(command -v apt-get)" ]; then \
		apt-get update && apt-get install -y wabt; \
	elif [ -x "$(command -v brew)" ]; then \
		brew install wabt; \
	else \
		echo "Please install WABT manually from https://github.com/WebAssembly/wabt"; \
	fi

.PHONY: all clean translate wasm run install-deps