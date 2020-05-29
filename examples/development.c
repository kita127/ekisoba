#define TRUE (1)
#define FALSE (0)
unsigned int if_gethan_0(int arg) {
  int res = FALSE;

  if (arg == 0) {
    res = 0;
  } else if (arg > 0) {
    res = 1;
  } else {
    res = 2;
  }

  return res;
}
