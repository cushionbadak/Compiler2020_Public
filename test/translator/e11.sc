struct list {
  int head;
  struct list * tail;
} list_one;

struct list list_two;


list_one.head = 1;
list_two.head = 2;

list_two.tail = malloc(2);

(*list_two.tail).head = 3;

list_one.head = (*list_two.tail).head;
