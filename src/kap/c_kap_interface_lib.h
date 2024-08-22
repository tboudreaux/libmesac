#ifndef c_kap_interface_lib_h
#define c_kap_interface_lib_h

#include "c_kap_interface_external.h"
#include "c_kap_interface_def.h"

#include <stdbool.h>

/*
@breif Initialize the mesa kap library, calling the mesa (fortran) kap_init function
@param use_cache (bool): whether to use the cache
@param kap_cache_dir (char*): the directory to store the cache
@param ierr (int): error code
*/
void kap_init(bool use_cache, char *kap_cache_dir, int *ierr);

/*
@breif Shutdown the mesa kap library, calling the mesa (fortran) kap_shutdown function
*/
void kap_shutdown();

/*
@breif Allocate a kap handle, calling the mesa (fortran) alloc_kap_handle function
@param ierr (int): error code
@return handle (int): the handle to the kap instance
*/
int alloc_kap_handle(int *ierr);

/*
@breif Allocate a kap handle using an inlist, calling the mesa (fortran) alloc_kap_handle_using_inlist function
@param inlist (char*): the inlist to use
@param ierr (int): error code
@return handle (int): the handle to the kap instance
*/
int alloc_kap_handle_using_inlist(char *inlist, int *ierr);

/*
@breif Initialize all of the libraries needed to use the kap module, calling the mesa (fortran) kap_init
    math_init, and chem_init subroutines
@param mesa_dir (char*): the directory to the mesa installation (environment variable MESA_DIR)
@param isotopes (char*): the isotopes datafile to use (i.e. isotopes.data)
@param use_cache (bool): whether to use the cache
@param kap_cache_dir (char*): the directory to store the cache
@param ierr (int): error code
*/
void mesa_kap_full_init(char *mesa_dir, char *isotopes, bool use_cache,
                            char *kap_cache_dir, int *ierr);

/*
@breif allocate the kap pointer which stores configuration info about 
    the instance of the kap handle, calling the mesa (fortran) kap_ptr function
@param handle (int): the handle to the kap instance
@param rq (c_Kap_General_Info*): the pointer to the kap general info struct.
    This struct is allocated in the fortran code and passed to the c code
    and it may be configures on the c or fortran side.
@param ierr (int): error code
*/
void kap_ptr(int handle, c_Kap_General_Info *rq, int *ierr);

/*
@breif setup the tables for the kap library, calling the mesa (fortran) kap_setup_tables function
@param handle (int): the handle to the kap instance
@param ierr (int): error code
*/
void kap_setup_tables(int handle, int *ierr);

/*
@breif setup the hooks for the kap library, calling the mesa (fortran) kap_setup_hooks function
@param handle (int): the handle to the kap instance
@param ierr (int): error code
*/
void kap_setup_hooks(int handle, int *ierr);

/*
@breif setup the kap library, calling the mesa (fortran) kap_setup function. Will call
    allocate_kap_handle_using_inlist, kap_ptr, kap_setup_tables, and kap_setup_hooks
@param inlist (char*): the inlist to use
@param ierr (int): error code
*/
int kap_setup(char *inlist, int *ierr);

/*
@breif get the number of species in the kap library, calling the mesa (fortran) get_num_kap_species function
@return num_species (int): the number of species
*/
int get_num_kap_fracs();

/*
@breif call the kap_get subroutine, the default kap function, calling the mesa (fortran) kap_get function
@param handle (int): the handle to the kap instance
@param species (int): the number of species to consider in the opacity calculations
@param chem_id (int*): An array of the chemical ids of the species
@param net_iso (int*): An array of the net isotopes of the species
@param xa (double*): An array of the mass fractions of the species
@param NSpec (int): the number of species
@param maxpts (int): the number of points to consider in the opacity calculations
@param logRho (double): the log of the density
@param logT (double): the log of the temperature
@param lnfree_e (double): the log of the free electron fraction
@param d_lnfree_e_dlnRho (double): the derivative of the log of the free electron fraction with respect to the log of the density
@param d_lnfree_e_dlnT (double): the derivative of the log of the free electron fraction with respect to the log of the temperature
@param eta (double): the electron degeneracy parameter
@param d_eta_dlnRho (double): the derivative of the electron degeneracy parameter with respect to the log of the density
@param d_eta_dlnT (double): the derivative of the electron degeneracy parameter with respect to the log of the temperature
@param kap_fracs (double*): Contriibutions to the net opacity per species
@param kap (double*): the net opacity
@param dlnkap_dlnRho (double*): the derivative of the log of the net opacity with respect to the log of the density
@param dlnkap_dlnT (double*): the derivative of the log of the net opacity with respect to the log of the temperature
@param dlnkap_dxa (double*): the derivative of the log of the net opacity with respect to the mass fractions
@param ierr (int): error code
*/
void kap_get(int handle, int species, int *chem_id, int *net_iso, double *xa,
             int NSpec, int maxpts, double logRho, double logT, double
             lnfree_e, double d_lnfree_e_dlnRho, double d_lnfree_e_dlnT, double
             eta, double d_eta_dlnRho, double d_eta_dlnT, double *kap_fracs,
             double *kap, double *dlnkap_dlnRho, double *dlnkap_dlnT, double
             *dlnkap_dxa, int *ierr);

#endif
