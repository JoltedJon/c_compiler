# Jon's C Compiler
This is my own personal work in progress C compiler written in C. 
The goal of this project is to eventually emit code to LLVM. I'm building this compiler to gain a better grasp of the C language both by using it and by writing the lexer and parser for it. 

# Setup

### Prerequisites
- [Meson build system](https://mesonbuild.com/index.html)

### Building and Running
```console
$ meson setup builddir
$ cd builddir
$ meson compile
$ ./jcc <C files>
```