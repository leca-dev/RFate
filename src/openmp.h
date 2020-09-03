#ifndef OPENMP_H
#define OPENMP_H

#if defined _OPENMP
  #include <omp.h>
#else
  inline int omp_get_max_threads() { return 1;}
  inline void omp_set_num_threads(int noCPU) {}
#endif

#endif
