#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include "c_kap_interface_lib.h"
#include "c_kap_interface_external.h"
#include "c_kap_interface_def.h"


int main(){
  printf("Calling kap_wrapper from C\n");
  int ierr;
  int handle;
  const char *mesa_dir_env_name = "MESA_DIR";
  char *mesa_dir = getenv(mesa_dir_env_name);
  mesa_microphysics_init(mesa_dir, "isotopes.data", false, "../../data/kap_data/cache/", &ierr);
  printf("mesa_microphysics_init ierr=%d\n", ierr);
  handle = kap_setup("inlist_sample", &ierr);

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

  printf("Mstar: %f\n", Mstar);

  // kap_get(handle, 0, NULL, NULL, NULL, 10.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  //         NULL, NULL, NULL, NULL, NULL, &ierr);
  return 0;
}
