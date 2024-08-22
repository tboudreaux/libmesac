#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include "c_kap_interface_lib.h"
#include "c_kap_interface_external.h"
#include "c_kap_interface_def.h"
#include "c_utils_interface_lib.h"
#include "c_chem_interface_lib.h"
#include "c_chem_interface_def.h"


int main(){
  printf("Testing Calling MESA/kap via C interface\n");
  int ierr;
  int handle;
  const char *mesa_dir_env_name = "MESA_DIR";
  char *mesa_dir = getenv(mesa_dir_env_name);

  // Intialize microphysics and numerical libraries needed to get opacities from MESA/kap
  mesa_kap_full_init(mesa_dir, "isotopes.data", false, "../../data/kap_data/cache/", &ierr);
  printf("mesa_microphysics_init ierr=%d\n", ierr);

  // Allocate the kappa handle and memory space from the inlist
  handle = kap_setup("inlist_sample", &ierr);

  // These should come from an EOS call; however, for now they are find here
  double lnfree_e = 0;
  double d_lnfree_e_dlnRho = 0;
  double d_lnfree_e_dlnT = 0;
  double eta = 0;
  double d_eta_dlnRho = 0;
  double d_eta_dlnT = 0;

  int NSpec = 31;
  int maxpts = 2000;
  int num_chem_isos;
  num_chem_isos = get_num_chem_isos();

  // Build the species list to compute opacities for
  c_chem_ids *cid = malloc(sizeof(c_chem_ids));
  get_some_isos(cid);
  int chem_id[31] = {cid->ih1, cid->ih2, cid->ihe3, cid->ili7, cid->ibe7, cid->ibe7, cid->ib8, cid->ib8,
    cid->ic12, cid->ic13, cid->in14, cid->in15, cid->io16, cid->io17, cid->io18, cid->if19,
    cid->ine20, cid->ine21, cid->ine22, cid->ina22, cid->ina23, cid->img24, cid->img25,
    cid->img26, cid->ial26, cid->ial27, cid->isi28, cid->isi29, cid->isi30, cid->ip31,
    cid->is32};
  int net_iso[num_chem_isos];
  for (int i = 0; i < num_chem_isos; i++){
    net_iso[i] = 0;
  }
  for (int i = 0; i < NSpec; i++){
    net_iso[chem_id[i]] = i+1;
  }
  free(cid);

  // Read in a example model from a model file (hardcoded filename)
  double Mstar;
  double Z_init;
  int Npts;
  int Nspec;
  double lnRho[maxpts];
  double lnT[maxpts];
  double lnR[maxpts];
  double L;
  double dq[maxpts];
  double X[NSpec][maxpts];

  simple_mesa_model_read("sample_kap_agb.model", &Mstar, &Z_init, &Npts, &Nspec,
                         lnRho, lnT, lnR, &L, dq, X, &ierr);

  printf("Selected Model params...\n");
  printf("\tMstar: %f\n", Mstar);
  printf("\tZ_init: %f\n", Z_init);
  printf("\tNpts: %d\n", Npts);
  printf("\tNspec: %d\n", Nspec);

  int num_kap_fracs = get_num_kap_fracs();

  double *kap_fracs = malloc(num_kap_fracs * sizeof(double));
  double kap;
  double dlnkap_dlnRho;
  double dlnkap_dlnT;
  double dlnkap_dxa[NSpec];

  double kap_expected = 0.237565;
  float threshold = 1e-5;

  kap_get(handle, Nspec, chem_id, net_iso, X, NSpec, maxpts, lnRho[0], lnT[0], lnfree_e,
          d_lnfree_e_dlnRho, d_lnfree_e_dlnT, eta, d_eta_dlnRho, d_eta_dlnT,
          kap_fracs, &kap, &dlnkap_dlnRho, &dlnkap_dlnT, dlnkap_dxa, &ierr);

  
  printf("kap: %lf\n", kap);
  
  // threshold test
  if (abs(kap - kap_expected) < threshold){
    printf("\e[1;32mkap test passed\n");
    free(kap_fracs);
    return 0;
  } else {
    printf("\e[1;31mkap test failed\e[0m\n");
    free(kap_fracs);
    return 1;
  }
  return 2;
}
