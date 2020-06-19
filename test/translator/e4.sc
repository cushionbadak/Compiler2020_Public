int[8] a;
int i;

for (i = 0; i < 8; i = i + 1)
{
  a[i] = random();
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
  }

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