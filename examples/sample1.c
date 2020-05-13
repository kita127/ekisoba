extern int external_var;

char g_char_var;
int g_int_var;
int g_int_var2 = 100;
int (*func_p)(int a, int b);

int sum_func(int a, int b) {
  int res;
  res = a + b;
  return res;
}
