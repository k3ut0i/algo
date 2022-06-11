#include<stdio.h>
#include<stdlib.h>

int int_cmp(int a, int b){
  if(a >= b) return 1;
  else return 0;
}

typedef int data_t;
typedef int(*cmp_fn_t)(data_t, data_t);

typedef struct binary_heap{
  int end; // array end pos
  int size; // total size of the underlying array
  data_t* data;
  cmp_fn_t cmp;
} *bh_t;

bh_t bh_new(int size, cmp_fn_t fn){
  bh_t h = malloc(sizeof(*h));
  h->end = 0, h->size = size, h->data = malloc(sizeof(data_t)*size), h->cmp = fn;
  return h;
}

void bh_destroy(bh_t h){
  free(h->data);
  free(h);
}
#define parent(i) (((i)-1)/2)
#define child_left(i) (2*(i)+1)
#define child_right(i) (2*(i+1))
#define swap(X, Y) { typeof(X) tmp = X; X = Y, Y = tmp; }
void bh_insert(bh_t h, data_t d){
  int i = h->end++;
  h->data[i] = d;
  while(i != 0 && !h->cmp(h->data[parent(i)], h->data[i])){
    swap(h->data[parent(i)], h->data[i]);
    i = parent(i);
  }
}
data_t bh_extract(bh_t h){
  data_t r = h->data[0];
  h->data[0] = h->data[--h->end];
  int i = 0, ip = -1;
  while(i != ip) {
    ip = i; int cl = child_left(i), cr = child_right(i);
    if(cl < h->end && !h->cmp(h->data[i], h->data[cl])) i = cl;
    if(cr < h->end && !h->cmp(h->data[i], h->data[cr])) i = cr;
    if (i != ip) swap(h->data[i], h->data[ip]);
  }
  return r;
}
void bh_delete(bh_t h, data_t d){
  
}

int main(){
  bh_t h = bh_new(10, int_cmp);
  int a[] = {9, 4, 2, 7, 1, 0, 8, 3, 6, 5};
  for(int i = 0; i < 10; i++) bh_insert(h, a[i]);
  for(int i = 0; i < 10; i++) fprintf(stdout, "%d ", bh_extract(h));
  bh_destroy(h);
  return 0;
}
