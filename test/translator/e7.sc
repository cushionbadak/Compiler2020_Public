int[8] arr;
int i;

for(i = 0; i < 8; i = i + 1)
{
  arr[i] = random();
  write(arr[i], 0);
  write(0, 1);
}
write(0, 2);

int f (int * v, int i)
{
  if (i < 0)
  {
    return 0;
  }

  write(v[i], 0);
  write(0, 1);

  f(v, (i - 1));

  return 0;
  
}

f(arr, 7);
write(0, 2);
