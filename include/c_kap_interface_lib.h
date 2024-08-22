#ifndef c_kap_interface_lib_h
#define c_kap_interface_lib_h

#include "c_kap_interface_external.h"
#include "c_kap_interface_def.h"

#include <stdbool.h>
#include <string.h>


void kap_init(bool use_cache, char *kap_cache_dir, int *ierr);

void kap_shutdown();

int alloc_kap_handle(int *ierr);

int alloc_kap_handle_using_inlist(char *inlist, int *ierr);

void mesa_microphysics_init(char *mesa_dir, char *isotopes, bool use_cache,
                            char *kap_cache_dir, int *ierr);

void kap_ptr(int handle, c_Kap_General_Info *rq, int *ierr);

void kap_setup_tables(int handle, int *ierr);

void kap_setup_hooks(int handle, int *ierr);

int kap_setup(char *inlist, int *ierr);

void kap_get(int handle, int species, int *chem_id, int *net_iso, double *xa,
             double logRho, double logT, double lnfree_e, double
             d_lnfree_e_dlnRho, double d_lnfree_e_dlnT, double eta, double
             d_eta_dlnRho, double d_eta_dlnT, double *kap_fracs, double *kap,
             double *dlnkap_dlnRho, double *dlnkap_dlnT, double *dlnkap_dxa,
             int *ierr);

void get_some_isos(c_chem_ids *chem_info);

int get_num_chem_isos();

int get_num_kap_fracs();

void simple_mesa_model_read(char *path, double *Mstar, double *Z_init, int
                            *Npts, int *Nspec, double *lnRho, double *lnT,
                            double *lnR, double *L, double *dq, double *X, int
                            *ierr);

#endif
