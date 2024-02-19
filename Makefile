all : test run

SRCS = $(wildcard src/*.zig)

test : zig-out/bin/test

run : zig-out/bin/yapl
	zig-out/bin/yapl -vv test.yapl

zig-out/bin/test : $(SRCS) Makefile
	zig build test -freference-trace=32 --color on --summary all

zig-out/bin/yapl : $(SRCS) Makefile
	zig build -freference-trace=32 --color on --summary all

