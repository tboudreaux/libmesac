!-----------------------------------------------------------------------
! Subroutine: c_get_some_isos
! Description:    Get the isotopes of the chemical element ids from MESA/chem
! Input:          c_chem_ptr - pointer to the chemical data structure
! Output:         chem_ids - pointer to the chemical element ids
!-----------------------------------------------------------------------
subroutine c_get_some_isos(c_chem_ptr) bind(C, name="c_get_some_isos")
  use chem_interface_def, only: c_set_some_isos, c_Chem_Ids
  use iso_c_binding, only: c_ptr, c_f_pointer
  type(c_ptr), value :: c_chem_ptr
  type(c_Chem_Ids), pointer :: chem_ids

  call c_f_pointer(c_chem_ptr, chem_ids)
  call c_set_some_isos(chem_ids)
end subroutine c_get_some_isos

!-----------------------------------------------------------------------
! Function: c_get_num_chem_isos
! Description:    Get the number of isotopes of the chemical elements from MESA/chem
! Output:         nci - number of isotopes
!-----------------------------------------------------------------------
integer function c_get_num_chem_isos() result(nci) bind(C, name="c_get_num_chem_isos")
  use chem_def, only: num_chem_isos
  nci = num_chem_isos
end function c_get_num_chem_isos 
