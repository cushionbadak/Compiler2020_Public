struct {
  int X;
  int Y;
} Z;

struct {int X; int Y;} f (int a, int b) {
  struct {int X; int Y;} V;

  V.X = a;
  V.Y = b;

  return V;
}

Z = f(100, 200);

write(Z.X, 0);
write(1, 1);
write(Z.Y, 0);
write(1, 2);

