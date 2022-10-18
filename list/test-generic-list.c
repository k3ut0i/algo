#include<stdio.h>
#include<stdlib.h>

#define DEFINE_LIST(name, type)                 \
  typedef struct name{                          \
    type car;                                   \
    struct name* cdr;                           \
  } *name##_t;                                  \
  name##_t cons_##name(type car, name##_t cdr){ \
    name##_t n = malloc(sizeof(*n));            \
    n->car = car, n->cdr = cdr;                 \
    return n;                                   \
  }                                             \
  void destroy_##name(name##_t l){              \
    while(l != NULL){                           \
      name##_t ln = l->cdr;                     \
      free(l);                                  \
      l = ln;                                   \
    }                                           \
  }                                             \

DEFINE_LIST(il, int); // Integer list named il_t

int main(){
  const int n = 10;
  il_t l = NULL;
  for(int i = 0; i < n; i++) l = cons_il(i, l);
  const il_t lc = l;
  while(l != NULL){
    fprintf(stdout, "%d\n", l->car);
    l = l->cdr;
  }
  destroy_il(lc);
  return 0;
}
