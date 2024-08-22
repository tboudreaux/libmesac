#ifndef c_chem_interface_lib_h
#define c_chem_interface_lib_h

#include "c_chem_interface_external.h"
#include "c_chem_interface_def.h"

#include <stdbool.h>

/*
@brief get species ids for all isotopes which mesa tracks
@param cid pointer to a c_chem_ids struct to be filled
*/
void get_some_isos(c_chem_ids *chem_info);

/*
@brief get number of isotopes which mesa tracks
@return number of isotopes
*/
int get_num_chem_isos();

#endif
