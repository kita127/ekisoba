/* Variable definition */
int var_non_init;
char var_c = 'k';
char var_a, Var_b = 100;
static unsigned int var_s_uint = 555;
char var_c;
unsigned char var_uc;
signed char var_sc;
short var_s;
unsigned short var_us;
signed short var_ss;
int var_i;
unsigned int var_ui;
signed int var_si;
long var_l;
unsigned long var_ul;
signed long var_sl;
long long var_ll;
unsigned long long var_ull;
signed long long var_sll;
float var_f;
double var_d;
signed long long var_sll;
char *var_p;
char **var_pp;
char ***var_ppp;

static int var_sti;
const int var_consi;
volatile int var_voli;

/* Struct */
struct St_tag {
  int menber_i;
  char menber_c;
};
struct St_tag str_var;

typedef struct St_tag St;
St str_var2;

/* Expression Initializer */
int var_exp = 100;
int var_exp2 = 1 + 1;
int var_exp3 = ((1 + 2) + (3 - 4)) * ((5 + 6) - (7 - 8));

/* Function definition */
void func(void) {}

int add(int a, int b) {
  int result;
  result = a + b;
  return result;
}
