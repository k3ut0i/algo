#include<stdio.h>
#include<stdlib.h>
typedef struct il{
  int car;
  struct il* cdr;
} *il_t;

il_t cons(int car, il_t cdr){
  il_t n = malloc(sizeof(*n));
  n->car = car, n->cdr = cdr;
  return n;
}

void destroy_il(il_t l){
  while(l != NULL){
    il_t n = l->cdr;
    free(l);
    l = n;
  }
}

struct graph{
  int nnodes;
  il_t* adjacency_list;
};

struct graph* graph_from_edges(int nnodes, int nedges, int* edges, int directedp){
  struct graph* g = malloc(sizeof(*g));
  g->nnodes = nnodes;
  g->adjacency_list = malloc(sizeof(il_t)*(nnodes+1));
  for(int i = 0; i <= nnodes; i++) g->adjacency_list[i] = NULL;
  for(int i = 0; i < nedges; i++){
    int v1 = edges[2*i], v2 = edges[2*i+1];
    g->adjacency_list[v1] = cons(v2, g->adjacency_list[v1]);
    if(!directedp)
      g->adjacency_list[v2] = cons(v1, g->adjacency_list[v2]);
  }
  return g;
}

void destroy_graph(struct graph* g){
  for(int i = 0; i <= g->nnodes; i++) destroy_il(g->adjacency_list[i]);
  free(g->adjacency_list);
  free(g);
}
void print_dfs_from(int n, int m, int* es, int s){
  struct graph* g = graph_from_edges(n, m, es, 0); // undirected
  int stack[n], sp = -1, mark[n+1];//, path[n], pi = -1;
  for(int i = 0; i <= n; i++) mark[i] = 0;
  stack[++sp] = s;
  while(sp >= 0){
    int cnode = stack[sp];
    if(mark[cnode]) sp--;
    else{
      il_t ns = g->adjacency_list[cnode];
      mark[cnode] = 1;
      fprintf(stdout, "%d ", cnode);
      while(ns!=NULL) {
	if(!mark[ns->car]) stack[++sp] = ns->car;
	ns = ns->cdr;
      }
    }
  }
  fprintf(stdout, "\n");
  destroy_graph(g);
}
int main(int argc, char* argv[]){
  if(argc != 2) {
    fprintf(stderr, "Usage: %s <node number>\n", argv[0]);
    return -1;
  }
  int source_node = atoi(argv[1]);
  int nnodes, nedges;
  fscanf(stdin, "%d %d", &nnodes, &nedges);
  int tmp;
  int edges[2*nedges];
  for(int i = 0; i < nedges; i++)
    fscanf(stdin, "%d %d %d", &edges[2*i], &edges[2*i+1], &tmp);
  print_dfs_from(nnodes, nedges, edges, source_node);
  return 0;
}
