// Expressions

void foo() {
  const char* str = "Hello World!";
  int i = 69;
  float k = 0.1;
  double j = .29;
  double x = 12345.6789;

  int expr = 1 || 2 + 3 + 4 + 5 + 6 + 8 &&
                      9 + 10 | 11 + 12 ^ 13 + 14 & 15 + 16 == 17 + 18 != 19 + 20 < 21 + 22 >> 23 + 24 * 25;
}