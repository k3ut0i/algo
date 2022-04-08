#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<assert.h>

static inline int next(int* S, int i , int n){ // find non zero index after i
  for(int j = i+1; j < n; j++)
    if(S[j]) return j;
  return -1; // Should never happen in linearsieve
}

//https://www.cs.utexas.edu/users/misra/scannedPdf.dir/linearSieve.pdf
int linearsieve(int n, int** primes){ // max:in, *primes:out, nprimes:ret
  int* S = malloc(sizeof(int)*(n+1));
  for(int i = 0; i <= n; i++) S[i] = i;
  int p = 2, pi = 2;
  while(p*p <= n){
    int q = p, qi = pi;
    while(p*q <= n){
      int x = p*q;
      while(x <= n)
	S[x] = 0, x = p*x;
      qi = next(S, qi, n-1);
      q = S[qi];
    }
    pi = next(S, pi, n-1);
    p = S[pi];
  }
  int count = 0;
  for(int i = 2; i <= n; i++) count += (S[i] == 0) ? 0 : 1; // I can just count removals in the main algo
  *primes = malloc(sizeof(int)*count);
  for(int i = 2, j = 0; i <= n; i++)
    if(S[i]) (*primes)[j++] = S[i];
  free(S);
  return count;
}

static inline int next1(int* S, int i , int n){ // find a prime index after i
  for(int j = i+1; j < n; j++)
    if(S[j] == j) return j;
  return -1; // Should never happen in linearsieve
}

int* smallest_prime_factor_ls(int n){ // adapting linear seive to get a prime factor
  int* S = malloc(sizeof(int)*(n+1));
  for(int i = 0; i <= n; i++) S[i] = i;
  int p = 2, pi = 2;
  while(p*p <= n){
    int q = p, qi = pi;
    while(p*q <= n){
      int x = p*q;
      while(x <= n)
	S[x] = p, x = p*x;
      qi = next1(S, qi, n-1);
      q = S[qi];
    }
    pi = next1(S, pi, n-1);
    p = S[pi];
  }
  return S;
}

int brute_sieve(int max, int** primes){ // max:in, *primes:out, nprimes:ret
  int default_size = max;
  int* primes_list = malloc(sizeof(int)*default_size);
  int primes_count = 0;
  primes_list[primes_count++] = 2;
  for(int i = 3; i <= max; i++){
    int primep = 1;
    for(int d = 2; d <= ceil(sqrt(i)); d++)
      if(i%d==0) { primep = 0; break; }
    if(primep) primes_list[primes_count++] = i;
  }
  *primes = malloc(sizeof(int)*primes_count);
  memcpy(*primes, primes_list, primes_count*sizeof(int));
  free(primes_list);
  return primes_count;
}

int test_linear(int max){
  int** primesb = malloc(sizeof(*primesb));
  int** primesl = malloc(sizeof(*primesl));
  int nprimesb = brute_sieve(max, primesb), nprimesl = linearsieve(max, primesl);
  assert(nprimesb == nprimesl);
  for(int i = 0; i < nprimesb; i++) assert((*primesb)[i] == (*primesl)[i]);
  return 0;
}

int test_prime_factor(int max){
  int* pf = smallest_prime_factor_ls(max);
  for(int i = 0; i <= max; i++) fprintf(stdout, "%d\n", pf[i]);
  return 0;
}

int main(int argc, char* argv[]){
  if(argc == 1){
    fprintf(stderr, "Usage: %s <type> <max_num>\n"
	    "type           0: brute force, 1: linear sieve\n"
	    , argv[0]);
    return -1;
  } else if(argc == 2){
    int num = atoi(argv[1]);
    test_linear(num);
    test_prime_factor(num);
    return 0;
  } else {
    int type = atoi(argv[1]);
    int num = atoi(argv[2]);
    int** primes;
    primes = malloc(sizeof(*primes));
    int nprimes = 0;
    if (type) nprimes = linearsieve(num, primes);
    else nprimes = brute_sieve(num, primes);
    fprintf(stdout, "Number of primes below %d is %d.\n", num, nprimes);
    for(int i = 0; i < nprimes; i++)
      fprintf(stdout, "%d\n", (*primes)[i]);
    free(*primes);
    free(primes);
    /* test_linear(num); */
    return 0;
  }
}
