
debug: build
	meson compile -C build jcc

build:
	meson setup build

valgrind: debug
	valgrind --leak-check=yes --log-file=valgrind.out --leak-check=full --show-leak-kinds=all ./build/jcc src/lexer.c > /dev/null

clean:
	rm -rf build

.PHONY: debug build valgrind clean
