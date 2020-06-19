int a;
int b;

int swap(int * x, int * y)
{
  int tmp;
  tmp = (*x);
  (*x) = (*y);
  (*y) = tmp;

  return 0;

}

a = 100;
b = 200;
swap((&a), (&b));