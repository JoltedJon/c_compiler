# Jon's C Compiler

This is my own personal work in progress C compiler written in C++20.
Currently the compiler has the capability of emitting to LLVM, output the AST, and also has an unfinished LC2K output.

The LLVM IR that is created goes through a couple basic optimizations to enable for better output.

There are many things that are not yet finished in this project, for instance structs/unions, and function pointers are not yet implemented. Arrays currently have no way to be outputted to LC2K yet. LC2K's output currently operates under the assumption of unlimited virtual registers, so register allocation needs to be finished, along with pushing Callee and Caller saved registers onto the stack.

No more work will be put onto this project unless if some interest is expressed for it to be completed.

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
