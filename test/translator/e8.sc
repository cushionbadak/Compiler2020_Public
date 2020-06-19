int answer;
int i;

answer = random();

i = read();

while (i != answer)
{
  if (i < answer)
  {
    write(11111, 0);
  }
  else
  {
    write(99999, 0);
  }

  write(0, 2);

  i = read();
}

write(123454321, 0);
write(0, 2);