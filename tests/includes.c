// clang-format off
#include "includes.h"
#include "includes.h"

// This test checks to make sure includes are working as intended to an extent
// It also tests to see if macros are passed in from the includes
// Also to see if macros are getting expanded

#define add(x, y) x + y

void foo() {
  struct my_struct s;
  s.a = 1;
  s.b = 2;
  s.c = MY_PI;

  int num = add(MY_PI, s.b);
}

#ifndef MY_PI
#error MY_PI not defined
#endif