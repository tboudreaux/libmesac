#ifndef C_EOS_INTERFACE_DEF_H
#define C_EOS_INTERFACE_DEF_H
#include <stdbool.h>

typedef struct {
  double Z_all_HELM;
  double logT_all_HELM;
  double logT_low_all_HELM;
  double coulomb_temp_cut_HELM, coulomb_den_cut_HELM;

  bool use_OPAL_SCVH;
  double logT_low_all_SCVH;
  double logT_all_OPAL;
  double logRho1_OPAL_SCVH_limit;
  double logRho2_OPAL_SCVH_limit;
  double logRho_min_OPAL_SCVH_limit;
  double logQ_max_OPAL_SCVH;
  double logQ_min_OPAL_SCVH;
  double Z_all_OPAL;

  bool use_FreeEOS;
  double logQ_max_FreeEOS_hi;
  double logQ_max_FreeEOS_lo;
  double logQ_min_FreeEOS_hi;
  double logQ_min_FreeEOS_lo;
  double logRho_max_FreeEOS_hi;
  double logRho_max_FreeEOS_lo;
  double logRho_min_FreeEOS_hi;
  double logRho_min_FreeEOS_lo;
  double logT_max_FreeEOS_hi;
  double logT_max_FreeEOS_lo;
  double logT_min_FreeEOS_hi;
  double logT_min_FreeEOS_lo;
  double logQ_cut_FreeEOS_lo_Z_max;
  double logQ_cut_lo_Z_FreeEOS_hi;
  double logQ_cut_lo_Z_FreeEOS_lo;
  double logQ_cut_hi_Z_FreeEOS_hi;
  double logQ_cut_hi_Z_FreeEOS_lo;
  double logRho_cut_FreeEOS_hi;
  double logRho_cut_FreeEOS_lo;
  double logT_cut_FreeEOS_hi;
  double logT_cut_FreeEOS_lo;
  char *suffic_for_FreeEOS_Z;

  bool use_CMS, CMS_use_fixed_composition;
  int CMS_fixed_composition_index;
  double max_Z_for_any_CMS, max_Z_for_all_CMS;
  double logQ_max_for_any_CMS, logQ_max_for_all_CMS;
  double logQ_min_for_any_CMS, logQ_min_for_all_CMS;
  double logRho_max_for_any_CMS, logRho_max_for_all_CMS;
  double logRho_min_for_any_CMS, logRho_min_for_all_CMS;
  double logT_max_for_any_CMS, logT_max_for_all_CMS;
  double logT_min_for_any_CMS, logT_min_for_all_CMS;
  double logT_max_for_any_CMS_pure_He, logT_max_for_all_CMS_pure_He;

  bool use_PC;
  double mass_fraction_limit_for_PC;
  double logRho1_PC_limit;
  double logRho2_PC_limit;
  bool PC_use_Gamma_limit_instead_of_T;
  double logT1_PC_limit;
  double logT2_PC_limit;
  double log_Gamma_e_all_HELM;
  double Gamma_e_all_HELM;
  double log_Gamma_e_all_PC;
  double tiny_fuzz;

  double PC_Gamma_start_crystal;
  double PC_Gamma_full_crystal;

  bool use_Skye;
  bool Skye_use_ion_offsets;
  double mass_fraction_limit_for_Skye;
  double Skye_min_gamma_for_solid;
  double Skye_max_gamma_for_liquid;
  char *Skye_solid_mixing_rule; // May be either 'Ogata' or 'PC'
  
  bool use_simple_Skye_blends;
  double logRho_min_for_any_Skye, logRho_min_for_all_Skye;
  double logT_min_for_any_Skye, logT_min_for_all_Skye;

  bool include_radiation, include_elec_pos;
  bool eosDT_use_linear_interp_for_X;
  bool eosDT_use_linear_interp_to_HELM;

  char *eosDT_file_prefix;

  bool okay_to_convert_ierr_to_skip;

  bool use_other_eos_component;
  bool use_other_eos_results;

  // Debugging Flags
  bool dbg;
  double logT_lo, logT_hi;
  double logRho_lo, logRho_hi;
  double X_lo, X_hi;
  double Z_lo, Z_hi;

  int handle;
  bool in_use;

  double eos_ctrl[10];
  int eos_integeral_ctrl[10];
  bool eos_logical_ctrl[10];

  char eos_char_ctrl[10][256];
} c_EoS_General_Info ;

void update_eos_ptr(int handle, c_EoS_General_Info *rq, int *ierr);

#endif

