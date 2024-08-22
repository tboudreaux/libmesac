#ifndef c_kap_interface_external_h
#define c_kap_interface_external_h

#include "c_kap_interface_def.h"

extern void c_kap_init(bool *use_cache, char **kap_cache_dir, int *ierr);
extern void c_kap_shutdown();
extern int c_alloc_kap_handle(int *ierr);
extern int c_alloc_kap_handle_using_inlist(char **inlist, int *inlist_len, int
                                           *ierr);
extern void c_mesa_kap_full_init(char **mesa_dir, char **isotopes, bool
                                     *use_cache, char **kap_cahce_dir, int
                                     *ierr, int *mesa_dir_len, int
                                     *isotopes_len, int *kap_cache_dir_len);
extern void c_kap_ptr(int *handle, c_Kap_General_Info *rq, int *ierr);
extern void c_kap_setup_tables(int *handle, int *ierr);
extern void c_kap_setup_hooks(int *handle, int *ierr);
extern void c_kap_get(int *handle, int *species, int *chem_id, int *net_iso,
                      double *xa, int *NSpec, int *maxpts, double *logRho,
                      double *logT, double *lnfree_e, double
                      *d_lnfree_e_dlnRho, double *d_lnfree_e_dlnT, double *eta,
                      double *d_eta_dlnRho, double *d_eta_dlnT, double
                      *kap_fracs, int *num_kap_fracs, double *kap, double
                      *dlnkap_dlnRho, double *dlnkap_dlnT, double *dlnkap_dxa,
                      int *ierr);
extern int c_get_num_kap_fracs();


#endif
