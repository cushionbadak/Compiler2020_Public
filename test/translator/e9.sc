switch (random() % 5) {
  case 1 : 
    write(1, 0);
  case 2 :
    write(2, 0);
    break;
  case 3 :
    write(3, 0);
  default :
    write(4, 0);
}

write(0, 2);