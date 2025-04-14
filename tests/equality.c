int is_positive(int x) { return (x & (1u << 31u)) == 0; }

int less_than(int x, int y) {
  int x_neg = !is_positive(x);
  int y_neg = !is_positive(y);

  int diff = x - y;

  return (x_neg && !y_neg) || (!(x_neg ^ y_neg) && !is_positive(diff));
}

int less_equal(int x, int y) { return less_than(x, y) || x == y; }

int greater_than(int x, int y) { return !less_equal(x, y); }

int greater_equal(int x, int y) { return !less_than(x, y); }

int equal(int x, int y) {
  int sub = x - y;
  return !sub;
}

int not_equal(int x, int y) {
  int sub = x - y;
  return !(!sub);
}