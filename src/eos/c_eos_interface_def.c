#include "c_eos_interface_def.h"
#include "c_eos_interface_external.h"

void update_eos_ptr(int handle, c_EoS_General_Info *rq, int *ierr) {
  c_update_eos_ptr(&handle, rq, ierr);
}
