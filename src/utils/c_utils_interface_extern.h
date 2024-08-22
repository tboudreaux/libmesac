#ifndef c_utils_interface_extern_h
#define c_utils_interface_extern_h

extern void c_simple_model_read(char **path, int *path_len, double *Mstar,
                                double *Z_init, int *Npts, int *Nspec, double
                                *lnRho, double *lnT, double *lnR, double *L,
                                double *dq, double *X, int *ierr);

#endif // c_utils_interface_extern_h
