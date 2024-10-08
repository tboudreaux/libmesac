#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include<gtest/gtest.h>

extern "C" {
  #include "c_kap_interface_lib.h"
  #include "c_kap_interface_external.h"
  #include "c_kap_interface_def.h"
  #include "c_utils_interface_lib.h"
  #include "c_chem_interface_lib.h"
  #include "c_chem_interface_def.h"
}

TEST(KapTests, KapGetWithinTolerance) {
  int ierr;
  int handle;
  const char *mesa_dir_env_name = "MESA_DIR";
  char *mesa_dir = getenv(mesa_dir_env_name);

  // Intialize microphysics and numerical libraries needed to get opacities from MESA/kap
  mesa_kap_full_init(mesa_dir, (char*)"isotopes.data", false, (char*)"../../data/kap_data/cache/", &ierr);

  // Allocate the kappa handle and memory space from the inlist
  handle = kap_setup((char*)"../share/kap_test_data/inlist_sample", &ierr);

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
  c_chem_ids *cid = (c_chem_ids *)malloc(sizeof(c_chem_ids));
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

  simple_mesa_model_read((char*)"../share/kap_test_data/sample_kap_agb.model", &Mstar, &Z_init, &Npts, &Nspec,
                         lnRho, lnT, lnR, &L, dq, &X[0][0], &ierr);

  int num_kap_fracs = get_num_kap_fracs();

  double *kap_fracs = (double *)malloc(num_kap_fracs * sizeof(double));
  double kap;
  double dlnkap_dlnRho;
  double dlnkap_dlnT;
  double dlnkap_dxa[NSpec];

  double kap_expected = 0.237565;
  float threshold = 1e-5;

  kap_get(handle, Nspec, chem_id, net_iso, &X[0][0], NSpec, maxpts, lnRho[0], lnT[0], lnfree_e,
          d_lnfree_e_dlnRho, d_lnfree_e_dlnT, eta, d_eta_dlnRho, d_eta_dlnT,
          kap_fracs, &kap, &dlnkap_dlnRho, &dlnkap_dlnT, dlnkap_dxa, &ierr);

  
  EXPECT_NEAR(kap, kap_expected, threshold);
  
  
}

int main(int argc, char **argv){
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
