int[3] a;
int *b;

b = malloc(3);
b[0] = 30;
b[1] = 300;
b[2] = 3000;
b[3] = 40000;

a[0] = b + 10000;
a[1] = (*b) + 20000;
a[2] = b[1] + 40000;