#define TRUE (1)
#define FALSE (0)
unsigned int if_gethan_0(int arg) {
  int res = FALSE;

  if (arg >= 0) {
    res = TRUE;
  } else {
    res = FALSE;
  }

  return res;
}
