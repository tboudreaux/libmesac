!------------------------------------------------------------------------------
!  module interface_def
!  This module contains libmesac/kap data structure definitions which are
!  built to be compatible with those in mesa/kap/kap_def.f90 while also
!  being compatible with C datatyps. This module also contains subroutines to convert
!  between the two data structures.
!------------------------------------------------------------------------------
module kap_interface_def

  use iso_c_binding, only: c_int, c_double, c_bool, c_char, c_ptr, c_f_pointer
  use chem_def

  !------------------------------------------------------------------------------
  !  Type definitions
  !------------------------------------------------------------------------------

  !------------------------------------------------------------------------------
  !  c_Kap_General_Info
  !  This type is a C compatible version of the Kap_General_Info type in kap_def.f90
  !------------------------------------------------------------------------------
  type, bind(C) :: c_Kap_General_Info
    real(c_double) :: Zbase
    integer(c_int) :: kap_option, kap_CO_option, kap_lowT_option
    real(c_double) :: kap_blend_logT_upper_bdy
    real(c_double) :: kap_blend_logT_lower_bdy
    logical(c_bool) :: cubic_interpolation_in_X
    logical(c_bool) :: cubic_interpolation_in_Z
    logical(c_bool) :: include_electron_conduction
    logical(c_bool) :: use_blouin_conductive_opacities
    logical(c_bool) :: use_Zbase_for_Type1
    logical(c_bool) :: use_Type2_opacities
    real(c_double) :: kap_Type2_full_off_X
    real(c_double) :: kap_Type2_full_on_X
    real(c_double) :: kap_Type2_full_off_dZ
    real(c_double) :: kap_Type2_full_on_dZ
    real(c_double) :: logT_Compton_blend_hi, logR_Compton_blend_lo
    logical(c_bool) :: show_info
    logical(c_bool) :: dbg
    real(c_double) :: logT_lo, logT_hi
    real(c_double) :: logRho_lo, logRho_hi
    real(c_double) :: X_lo, X_hi
    real(c_double) :: Z_lo, Z_hi
    integer(c_int) :: handle
    logical(c_bool) :: in_use
    real(c_double) :: kap_ctrl(10)
    integer(c_int) :: kap_integer_ctrl(10)
    logical(c_bool) :: kap_logical_ctrl(10)
    character(kind=c_char, len=1) :: kap_character_ctrl(10)
    integer(c_int) :: kap_character_ctrl_len
  end type c_Kap_General_Info

  contains    

  !------------------------------------------------------------------------------
  ! Subroutines
  !------------------------------------------------------------------------------

  !------------------------------------------------------------------------------
  ! Subroutine: c_kap_ptr_2_f_kap_ptr
  !  This subroutine converts a C pointer to a c_Kap_General_Info type to a
  !  Fortran pointer to a Kap_General_Info type.
  ! Parameters:
  !  c_rq_ptr: A C pointer to a c_Kap_General_Info type
  !  frq: A Fortran pointer to a Kap_General_Info type
  !  ierr: An integer error code
  !------------------------------------------------------------------------------
  subroutine c_kap_ptr_2_f_kap_ptr(c_rq_ptr, frq, ierr)
    use kap_def, only: Kap_General_Info, get_kap_ptr, kap_is_initialized

    implicit none
    type (c_ptr), value :: c_rq_ptr
    type (c_Kap_General_Info), pointer :: rq
    type(Kap_General_Info), pointer :: frq
    integer, intent(out) :: ierr

    integer :: i

    call c_f_pointer(c_rq_ptr, rq)
    if (.not. associated(rq)) then
      print *, "Error (libmesac:c_kap_interface_def). Pointer association failed."
      ierr = 1
      return
    end if

    frq%Zbase = rq%Zbase
    frq%kap_option = rq%kap_option
    frq%kap_CO_option = rq%kap_CO_option
    frq%kap_lowT_option = rq%kap_lowT_option
    frq%kap_blend_logT_upper_bdy = rq%kap_blend_logT_upper_bdy
    frq%kap_blend_logT_lower_bdy = rq%kap_blend_logT_lower_bdy
    frq%cubic_interpolation_in_X = rq%cubic_interpolation_in_X
    frq%cubic_interpolation_in_Z = rq%cubic_interpolation_in_Z
    frq%include_electron_conduction = rq%include_electron_conduction
    frq%use_blouin_conductive_opacities = rq%use_blouin_conductive_opacities
    frq%use_Zbase_for_Type1 = rq%use_Zbase_for_Type1
    frq%use_Type2_opacities = rq%use_Type2_opacities
    frq%kap_Type2_full_off_X = rq%kap_Type2_full_off_X
    frq%kap_Type2_full_on_X = rq%kap_Type2_full_on_X
    frq%kap_Type2_full_off_dZ = rq%kap_Type2_full_off_dZ
    frq%kap_Type2_full_on_dZ = rq%kap_Type2_full_on_dZ
    frq%logT_Compton_blend_hi = rq%logT_Compton_blend_hi
    frq%logR_Compton_blend_lo = rq%logR_Compton_blend_lo
    frq%show_info = rq%show_info
    frq%dbg = rq%dbg
    frq%logT_lo = rq%logT_lo
    frq%logT_hi = rq%logT_hi
    frq%logRho_lo = rq%logRho_lo
    frq%logRho_hi = rq%logRho_hi
    frq%X_lo = rq%X_lo
    frq%X_hi = rq%X_hi
    frq%Z_lo = rq%Z_lo
    frq%Z_hi = rq%Z_hi
    frq%handle = rq%handle
    frq%in_use = rq%in_use
    frq%kap_ctrl = rq%kap_ctrl
    frq%kap_integer_ctrl = rq%kap_integer_ctrl
    frq%kap_logical_ctrl = rq%kap_logical_ctrl

    do i = 1, 10
      frq%kap_character_ctrl(i) = rq%kap_character_ctrl(i)
    end do
  end subroutine c_kap_ptr_2_f_kap_ptr

  !------------------------------------------------------------------------------
  ! Subroutine: f_kap_ptr_2_c_kap_ptr
  !  This subroutine converts a Fortran pointer to a Kap_General_Info type to a
  !  C pointer to a c_Kap_General_Info type.
  ! Parameters:
  !  c_rq_ptr: A C pointer to a c_Kap_General_Info type
  !  frq: A Fortran pointer to a Kap_General_Info type
  !  ierr: An integer error code
  !------------------------------------------------------------------------------
  subroutine f_kap_ptr_2_c_kap_ptr(c_rq_ptr, frq, ierr)
    use kap_def, only: Kap_General_Info, get_kap_ptr, kap_is_initialized

    implicit none
    type (c_ptr), value :: c_rq_ptr
    type (c_Kap_General_Info), pointer :: rq
    type(Kap_General_Info), pointer :: frq
    integer, intent(out) :: ierr

    integer :: i

    call c_f_pointer(c_rq_ptr, rq)
    if (.not. associated(rq)) then
      print *, "Error! Pointer association failed. c kap pointer -> fortran kap pointer"
      ierr = 1
      return
    end if

    rq%Zbase = frq%Zbase
    rq%kap_option = frq%kap_option
    rq%kap_CO_option = frq%kap_CO_option
    rq%kap_lowT_option = frq%kap_lowT_option
    rq%kap_blend_logT_upper_bdy = frq%kap_blend_logT_upper_bdy
    rq%kap_blend_logT_lower_bdy = frq%kap_blend_logT_lower_bdy
    rq%cubic_interpolation_in_X = frq%cubic_interpolation_in_X
    rq%cubic_interpolation_in_Z = frq%cubic_interpolation_in_Z
    rq%include_electron_conduction = frq%include_electron_conduction
    rq%use_blouin_conductive_opacities = frq%use_blouin_conductive_opacities
    rq%use_Zbase_for_Type1 = frq%use_Zbase_for_Type1
    rq%use_Type2_opacities = frq%use_Type2_opacities
    rq%kap_Type2_full_off_X = frq%kap_Type2_full_off_X
    rq%kap_Type2_full_on_X = frq%kap_Type2_full_on_X
    rq%kap_Type2_full_off_dZ = frq%kap_Type2_full_off_dZ
    rq%kap_Type2_full_on_dZ = frq%kap_Type2_full_on_dZ
    rq%logT_Compton_blend_hi = frq%logT_Compton_blend_hi
    rq%logR_Compton_blend_lo = frq%logR_Compton_blend_lo
    rq%show_info = frq%show_info
    rq%dbg = frq%dbg
    rq%logT_lo = frq%logT_lo
    rq%logT_hi = frq%logT_hi
    rq%logRho_lo = frq%logRho_lo
    rq%logRho_hi = frq%logRho_hi
    rq%X_lo = frq%X_lo
    rq%X_hi = frq%X_hi
    rq%Z_lo = frq%Z_lo
    rq%Z_hi = frq%Z_hi
    rq%handle = frq%handle
    rq%in_use = frq%in_use
    rq%kap_ctrl = frq%kap_ctrl
    rq%kap_integer_ctrl = frq%kap_integer_ctrl
    rq%kap_logical_ctrl = frq%kap_logical_ctrl

    do i = 1, 10
      rq%kap_character_ctrl(i) = frq%kap_character_ctrl(i)
    end do
  end subroutine f_kap_ptr_2_c_kap_ptr

end module kap_interface_def
