# Jon's C Compiler

This is my own personal work in progress C compiler written in C++20.
The goal of this project is to eventually emit code to LLVM. I'm building this compiler to gain a better grasp of the C language by writing the lexer and parser for it.
I also want a better grasp of how compilers work and what makes a good design.

# Setup

### Prerequisites

- [Meson build system](https://mesonbuild.com/index.html)
- [LLVM](https://llvm.org/)

### Building and Running

```console
$ meson setup build
$ cd build
$ meson compile
$ ./jcc <C files>
```
