#ifndef c_utils_interface_lib_h
#define c_utils_interface_lib_h

void simple_mesa_model_read(char *path, double *Mstar, double *Z_init, int
                            *Npts, int *Nspec, double *lnRho, double *lnT,
                            double *lnR, double *L, double *dq, double *X, int
                            *ierr);

#endif
