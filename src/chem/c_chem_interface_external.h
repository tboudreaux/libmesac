#ifndef c_chem_interface_external_h
#define c_chem_interface_external_h

#include "c_chem_interface_def.h"

/*
@brief get species ids for all isotopes which mesa tracks (bound to external fortran subroutine)
@param cid pointer to a c_chem_ids struct to be filled
*/
extern void c_get_some_isos(c_chem_ids *cid);

/*
@brief get number of isotopes which mesa tracks (bound to external fortran subroutine)
@return number of isotopes
*/
extern int c_get_num_chem_isos();

#endif
