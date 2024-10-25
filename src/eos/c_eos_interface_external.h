#ifndef c_eos_interface_external_h
#define c_eos_interface_external_h

#include "c_eos_interface_def.h"


extern void c_eos_init(bool *use_cache, char **eos_cache_dir, int *ierr);

extern void c_eos_shutdown();

extern int c_alloc_eos_handle(int *ierr);

extern int c_alloc_eos_handle_using_inlist(char **inlist, int *inlist_len, int *ierr);

extern void c_free_eos_handle(int *handle, int *ierr);

extern void c_eos_ptr(int *handle, c_EoS_General_Info *rq, int *ierr);

extern void c_update_eos_ptr(int *handle, c_EoS_General_Info *rq, int *ierr);

extern void c_eosDT_get(int *handle, int *species, int **chem_id, int **net_iso, double **xa,
               double *Rho, double *logRho, double *T, double *logT, double **res,
               double **d_dlnd, double **d_dlnT, double **d_dxa, int *ierr);

#endif
