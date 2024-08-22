#include "c_chem_interface_def.h"
#include "c_chem_interface_external.h"
#include "c_chem_interface_lib.h"

void get_some_isos(c_chem_ids *cid){
  c_get_some_isos(cid);
}

int get_num_chem_isos(){
  return c_get_num_chem_isos();
}