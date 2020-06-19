int[8] a;
int i;

a[0] = 561;
a[1] = 61;
a[2] = 888;
a[3] = 525;
a[4] = 76;
a[5] = 903;
a[6] = 134;
a[7] = 505;


for (i = 0; i < 8; i = i + 1)
{
  
  write(a[i], 0);
  write(0, 1);    /* second_argument 1 whitespace */
}
write(0, 2);      /* second_argument 2 newline    */

int quicksort (int* v, int low, int high) 
{
  int pivot;
  int l;
  int h;
  int tmp;

  /* DEBUG initial conditions */
  write(0, 2);
  for (i = 0; i < 8; i = i + 1)
  {
    write(a[i], 0);
    write(0, 1);    /* second_argument 1 whitespace */
  }
  write(0, 2);
  write(low, 0);
  write(0, 1);
  write(high, 0);
  write(0, 1);
  write(0, 2);

  if (low >= high) 
  {
    return 0;
  }

  if ((low + 1) == high)
  {
    if(v[low] > v[high])
    {
      pivot = v[low];
      v[low] = v[high];
      v[high] = pivot;
    }
    
    return 0;
  }

  /* initialize */
  pivot = v[low];
  l = low + 1;
  h = high;

  /* loop */
  while (l < h)
  {
    /* move low high pointers */
    while ((v[l] < pivot) && (l < high))  { l = l + 1; }
    while ((v[h] >= pivot) && (h > low)) { h = h - 1; }

    if (l >= h) 
    {
      break;
    }

    tmp = v[l];
    v[l] = v[h];
    v[h] = tmp;

    /* DEBUG in-loop condition */
    write(l, 0);
    write(0, 1);
    write(h, 0);
    write(0, 2);
    for (i = 0; i < 8; i = i + 1)
    {
      write(a[i], 0);
      write(0, 1);    /* second_argument 1 whitespace */
    }
    write(0, 2);
  }

  /* DEBUG outer-loop condition */
  write(l, 0);
  write(0, 1);
  write(h, 0);
  write(0, 1);
  write(pivot, 0);
  write(0, 2);

  /* change pivot and v_h ___ since v_l will be greater than v_h */
  if (h > low)
  {
    v[low] = v[h];
    v[h] = pivot;
    
    quicksort (v, low, (h - 1));
    if (h < high)
    {
      quicksort (v, (h + 1), high);
    }
  }
  else
  {
    quicksort (v, (low + 1), high);
  }


  
  return 0;
}

quicksort(a, 0, 7);

for (i = 0; i < 8; i = i + 1)
{
  write(a[i], 0);
  write(0, 1);    /* second_argument 1 whitespace */
}
write(0, 2);      /* second_argument 2 newline    */