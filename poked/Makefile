
B ?= ./build

CFLAGS += -D_GNU_SOURCE -Wall -Wextra -g3

FUSE_CFLAGS = $(shell pkg-config --cflags fuse3)
FUSE_LDFLAGS = $(shell pkg-config --libs fuse3)

USOCK_SRC = usock_buf.c usock.c
USOCK_HDR = usock_buf.h usock_buf-priv.h usock.h

.PHONY: all
all: $(B)/usock_buf-test $(B)/poked $(B)/poke-in $(B)/poke-repl
all: $(B)/poke-disasm $(B)/poke-fuse

$(B)/usock_buf-test: usock_buf-test.c usock_buf.c usock_buf.h usock_buf-priv.h
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) -o $@ usock_buf-test.c usock_buf.c $(LDFLAGS)

$(B)/poked: poked.c $(USOCK_SRC) $(USOCK_HDR)
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) -o $@ poked.c $(USOCK_SRC) $(LDFLAGS) -lpoke -pthread

$(B)/poke-in: poke-in.c 
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) -o $@ poke-in.c $(LDFLAGS)

$(B)/poke-repl: poke-repl.c linenoise.c linenoise.h
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) -o $@ poke-repl.c linenoise.c $(LDFLAGS)

$(B)/poke-disasm: poke-disasm.c
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) -o $@ poke-disasm.c $(LDFLAGS) -lcapstone

$(B)/poke-fuse: poke-fuse.c poke-fuse_helpers.h read-file.c read-file.h
	[ -d $(B) ] || mkdir -p $(B)
	$(CC) $(CFLAGS) $(FUSE_CFLAGS) -o $@ poke-fuse.c read-file.c \
		$(LDFLAGS) $(FUSE_LDFLAGS)

.PHONY: fmt
fmt:
	clang-format -i -style=gnu *.[ch]

.PHONY: clean
clean:
	rm -rf "$(B)"

