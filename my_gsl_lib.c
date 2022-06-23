#include <stdio.h>
#include <math.h>
#include <gsl/gsl_rng.h>

gsl_rng* init_gsl_rng() {
  gsl_rng* r;
  gsl_rng_env_setup();
  const gsl_rng_type * T;
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  return r;    
}

// cc -shared -fPIC my_gsl_lib.c -o my_gsl_lib.so -lm -lgsl