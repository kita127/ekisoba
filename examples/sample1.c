/* prototype declare */
int sum_func(int a, int b);

/* extern declare */
extern int external_var;

/* variable definition */
/* non initial value */
char g_char_var;
int g_int_var;

/* initial value */
int g_int_var2 = 100;

/* function pointer */
int (*func_p)(int a, int b);

/* function definition */
int sum_func(int a, int b) {
  int res;
  res = a + b;
  return res;
}
