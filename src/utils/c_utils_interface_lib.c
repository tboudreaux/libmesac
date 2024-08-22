#include "c_utils_interface_lib.h"

void simple_mesa_model_read(char *path, double *Mstar, double *Z_init, int
                            *Npts, int *Nspec, double *lnRho, double *lnT,
                            double *lnR, double *L, double *dq, double *X, int *ierr){

  int path_len = strlen(path);
  c_simple_model_read(&path, &path_len, Mstar, Z_init, Npts, Nspec, lnRho, lnT, lnR, L, dq, X, ierr);
}