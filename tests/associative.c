int foo() {
  int i1 = 1 + 2 * 3;
  int i2 = (1 + 2) * 3;
  int i3 = 4 / 2 + 3;
  int i4 = 4 / (2 + 2);

  int i5 = 1 - 2 - 3;
  int i6 = 4 / 2 / 2;
  int i7 = 1 << 2 << 3;

  i1 = i2 = i3 = i4 = i5 = i6 = i7;

  i1 += i2 -= i3 * i4;
  // (i2 == i3) ? (i2 == i4 ? (i2 == i5 ? 3 : 2) : 1) : 0
  i1 = i2 == i3 ? i2 == i4 ? i2 == i5 ? 3 : 2 : 1 : 0;

  return i1;
}