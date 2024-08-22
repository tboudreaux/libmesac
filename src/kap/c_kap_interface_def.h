#ifndef C_KAP_INTERFACE_DEF_H
#define C_KAP_INTERFACE_DEF_H
#include <stdbool.h>

typedef struct {
    double Zbase;
    int kap_option, kap_CO_option, kap_lowT_option;
    double kap_blend_logT_upper_bdy;
    double kap_blend_logT_lower_bdy;
    bool cubic_interpolation_in_X;
    bool cubic_interpolation_in_Z;
    bool include_electron_conduction;
    bool use_blouin_conductive_opacities;
    bool use_Zbase_for_Type1;
    bool use_Type2_opacities;
    double kap_Type2_full_off_X;
    double kap_Type2_full_on_X;
    double kap_Type2_full_off_dZ;
    double kap_Type2_full_on_dZ;
    double logT_Compton_blend_hi, logR_Compton_blend_lo;
    bool show_info;
    bool dbg;
    double logT_lo, logT_hi;
    double logRho_lo, logRho_hi;
    double X_lo, X_hi;
    double Z_lo, Z_hi;
    int handle;
    bool in_use;
    double kap_ctrl[10];
    int kap_integer_ctrl[10];
    bool kap_logical_ctrl[10];
    char kap_character_ctrl[10][256];  // Fixed-size strings in a 2D array
    int kap_charecter_ctrl_len[10];
} c_Kap_General_Info;

#endif

