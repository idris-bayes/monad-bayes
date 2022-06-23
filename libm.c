#include <stdio.h>
#include <math.h>
#include <gsl/gsl_rng.h>

double call_uniform(const gsl_rng* r){
  return gsl_rng_uniform(r);
}

gsl_rng* init_gsl_rng() {
  gsl_rng* r;
  gsl_rng_env_setup();
  const gsl_rng_type * T;
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  return r;    
}

// cc -shared -fPIC libm.c -o libm.so -lm -lgsl