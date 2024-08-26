#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "c_kap_interface_def.h"
#include "c_kap_interface_external.h"
#include "c_kap_interface_lib.h"


void kap_init(bool use_cache, char *kap_cache_dir, int *ierr){
  c_kap_init(&use_cache, &kap_cache_dir, ierr);
}

void kap_shutdown(){
  c_kap_shutdown();
}

int alloc_kap_handle(int *ierr){
  return c_alloc_kap_handle(ierr);
}

int alloc_kap_handle_using_inlist(char *inlist, int *ierr){
  int inlist_len = strlen(inlist);
  return c_alloc_kap_handle_using_inlist(&inlist, &inlist_len, ierr);
}

void mesa_kap_full_init(char *mesa_dir, char *isotopes, bool use_cache, char *kap_cache_dir, int *ierr){
  int mesa_dir_len = strlen(mesa_dir);
  int isotopes_len = strlen(isotopes);
  int kap_cache_dir_len = strlen(kap_cache_dir);
  c_mesa_kap_full_init(&mesa_dir, &isotopes, &use_cache, &kap_cache_dir, ierr, &mesa_dir_len, &isotopes_len, &kap_cache_dir_len);
}

void kap_prt(int handle, c_Kap_General_Info *rq, int *ierr){
  c_kap_ptr(&handle, rq, ierr);
}

void kap_setup_tables(int handle, int *ierr){
  c_kap_setup_tables(&handle, ierr);
}

void kap_setup_hooks(int handle, int *ierr){
  c_kap_setup_hooks(&handle, ierr);
}

int kap_setup(char *inlist, int *ierr){
  int handle = alloc_kap_handle_using_inlist(inlist, ierr);
  c_Kap_General_Info *rq = malloc(sizeof(c_Kap_General_Info));
  kap_prt(handle, rq, ierr);
  kap_setup_tables(handle, ierr);
  kap_setup_hooks(handle, ierr);
  return handle;
  // TODO Need to free rq
}

void kap_get(int handle, int species, int *chem_id, int *net_iso, double *xa,
             int NSpec, int maxpts, double logRho, double logT, double
             lnfree_e, double d_lnfree_e_dlnRho, double d_lnfree_e_dlnT, double
             eta, double d_eta_dlnRho, double d_eta_dlnT, double *kap_fracs,
             double *kap, double *dlnkap_dlnRho, double *dlnkap_dlnT, double
             *dlnkap_dxa, int *ierr)
{
  int num_kap_fracs = 0;

  c_kap_get(
    &handle,
    &species,
    chem_id,
    net_iso,
    xa,
    &NSpec,
    &maxpts,
    &logRho,
    &logT,
    &lnfree_e,
    &d_lnfree_e_dlnRho,
    &d_lnfree_e_dlnT,
    &eta, 
    &d_eta_dlnRho,
    &d_eta_dlnT,
    kap_fracs,
    &num_kap_fracs,
    kap,
    dlnkap_dlnRho,
    dlnkap_dlnT,
    dlnkap_dxa,
    ierr
  );
}

int get_num_kap_fracs(){
  return c_get_num_kap_fracs();
}
