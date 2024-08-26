!------------------------------------------------------------------------------
!  module eos_interface_def
!  This module contains libmesac/eos ata structure definitions which are
!  built to be compatible with those in mesa/eos/eos_def.f90 while also
!  being compatible with C datatyps. This module also contains subroutines to convert
!  between the two data structures.
!------------------------------------------------------------------------------
module eos_interface_def
  use const_def, only: strlen
  use iso_c_binding, only: c_int, c_double, c_bool, c_char, c_ptr, c_f_pointer
  use eos_def, only: num_FreeEOS_Zs

  !------------------------------------------------------------------------------
  !  Type definitions
  !------------------------------------------------------------------------------
  type, bind(C) :: c_EoS_General_Info

     ! limits for HELM
     real(c_double) :: Z_all_HELM ! all HELM for Z >= this unless eos_use_FreeEOS
     real(c_double) :: logT_all_HELM ! all HELM for lgT >= this
     real(c_double) :: logT_low_all_HELM ! all HELM for lgT <= this
     real(c_double) :: coulomb_temp_cut_HELM, coulomb_den_cut_HELM
     
     ! limits for OPAL_SCVH
     logical(c_bool) :: use_OPAL_SCVH
     real(c_double) :: logT_low_all_SCVH ! SCVH for lgT >= this
     real(c_double) :: logT_all_OPAL ! OPAL for lgT <= this
     real(c_double) :: logRho1_OPAL_SCVH_limit ! don't use OPAL_SCVH for logRho > this
     real(c_double) :: logRho2_OPAL_SCVH_limit ! full OPAL_SCVH okay for logRho < this
     real(c_double) :: logRho_min_OPAL_SCVH_limit ! no OPAL/SCVH for logRho < this
     real(c_double) :: logQ_max_OPAL_SCVH ! no OPAL/SCVH for logQ > this
     real(c_double) :: logQ_min_OPAL_SCVH ! no OPAL/SCVH for logQ <= this.
     real(c_double) :: Z_all_OPAL ! all OPAL for Z <= this
     
     ! limits for FreeEOS
     logical(c_bool) :: use_FreeEOS
     real(c_double) :: logQ_max_FreeEOS_hi
     real(c_double) :: logQ_max_FreeEOS_lo
     real(c_double) :: logQ_min_FreeEOS_hi
     real(c_double) :: logQ_min_FreeEOS_lo
     real(c_double) :: logRho_min_FreeEOS_hi
     real(c_double) :: logRho_min_FreeEOS_lo
     real(c_double) :: logRho_max_FreeEOS_hi
     real(c_double) :: logRho_max_FreeEOS_lo
     real(c_double) :: logT_min_FreeEOS_hi
     real(c_double) :: logT_min_FreeEOS_lo
     real(c_double) :: logT_max_FreeEOS_hi
     real(c_double) :: logT_max_FreeEOS_lo
     real(c_double) :: logQ_cut_FreeEOS_lo_Z_max
     real(c_double) :: logQ_cut_lo_Z_FreeEOS_hi
     real(c_double) :: logQ_cut_lo_Z_FreeEOS_lo
     real(c_double) :: logQ_cut_hi_Z_FreeEOS_hi
     real(c_double) :: logQ_cut_hi_Z_FreeEOS_lo
     real(c_double) :: logRho_cut_FreeEOS_hi
     real(c_double) :: logRho_cut_FreeEOS_lo
     real(c_double) :: logT_cut_FreeEOS_hi
     real(c_double) :: logT_cut_FreeEOS_lo
     character (kind=c_char, len=1) :: suffix_for_FreeEOS_Z(num_FreeEOS_Zs)
     
     ! limits for CMS
     logical(c_bool) :: use_CMS, CMS_use_fixed_composition
     integer(c_int) :: CMS_fixed_composition_index ! in [0,10]
     real(c_double) :: max_Z_for_any_CMS, max_Z_for_all_CMS ! set to -1 to disable CMS
     real(c_double) :: logQ_max_for_any_CMS, logQ_max_for_all_CMS      ! for upper blend zone in logQ = logRho - 2*logT + 12
     real(c_double) :: logQ_min_for_any_CMS, logQ_min_for_all_CMS      ! for lower blend zone in logQ
     real(c_double) :: logRho_max_for_all_CMS, logRho_max_for_any_CMS  ! for upper blend zone in logRho
     real(c_double) :: logRho_min_for_all_CMS, logRho_min_for_any_CMS  ! for lower blend zone in logRho
     real(c_double) :: logT_max_for_all_CMS, logT_max_for_any_CMS      ! for upper blend zone in logT
     real(c_double) :: logT_min_for_all_CMS, logT_min_for_any_CMS      ! for lower blend zone in logT      
     real(c_double) :: logT_max_for_all_CMS_pure_He, logT_max_for_any_CMS_pure_He ! upper logT blend zone is different for pure He
     
     ! limits for PC
     logical(c_bool) :: use_PC
     real(c_double) :: mass_fraction_limit_for_PC ! skip any species with abundance < this
     real(c_double) :: logRho1_PC_limit ! okay for pure PC for logRho > this
     real(c_double) :: logRho2_PC_limit ! don't use PC for logRho < this (>= 2.8)
     logical(c_bool) :: PC_use_Gamma_limit_instead_of_T
     real(c_double) :: logT1_PC_limit ! okay for pure PC for logT < this (like logT_all_OPAL)
     real(c_double) :: logT2_PC_limit ! don't use PC for logT > this (like logT_all_HELM)
     real(c_double) :: log_Gamma_e_all_HELM ! HELM for log_Gamma_e <= this
     real(c_double) :: Gamma_e_all_HELM ! 10**log_Gamma_e_all_HELM
     real(c_double) :: log_Gamma_e_all_PC ! PC for log_Gamma_e >= this
     real(c_double) :: tiny_fuzz
     ! crystallization boundaries
     real(c_double) :: PC_Gamma_start_crystal ! Begin releasing latent heat of crystallization
     real(c_double) :: PC_Gamma_full_crystal ! Fully into the solid phase

     ! limits for Skye
     logical(c_bool) :: use_Skye
     logical(c_bool) :: Skye_use_ion_offsets
     real(c_double) :: mass_fraction_limit_for_Skye
     real(c_double) :: Skye_min_gamma_for_solid ! The minimum Gamma_i at which to use the solid free energy fit (below this, extrapolate).
     real(c_double) :: Skye_max_gamma_for_liquid ! The maximum Gamma_i at which to use the liquid free energy fit (above this, extrapolate).
     character(kind=c_char, len=1) :: Skye_solid_mixing_rule ! Currently support 'Ogata' or 'PC'

     logical(c_bool) :: use_simple_Skye_blends
     real(c_double) :: logRho_min_for_any_Skye, logRho_min_for_all_Skye
     real(c_double) :: logT_min_for_any_Skye, logT_min_for_all_Skye

     ! misc
     logical(c_bool) :: include_radiation, include_elec_pos
     logical(c_bool) :: eosDT_use_linear_interp_for_X
     logical(c_bool) :: eosDT_use_linear_interp_to_HELM
  
     character(kind=c_char, len=1) :: eosDT_file_prefix

     logical(c_bool) :: okay_to_convert_ierr_to_skip
     
     ! other eos
     logical(c_bool) :: use_other_eos_component
     logical(c_bool) :: use_other_eos_results
     
     ! debugging
     logical(c_bool) :: dbg
     real(c_double) :: logT_lo, logT_hi
     real(c_double) :: logRho_lo, logRho_hi
     real(c_double) :: X_lo, X_hi
     real(c_double) :: Z_lo, Z_hi
     
     ! bookkeeping
     integer(c_int) :: handle
     logical(c_bool) :: in_use
     
     ! User supplied inputs
     real(c_double) :: eos_ctrl(10)
     integer(c_int) :: eos_integer_ctrl(10)
     logical(c_bool) :: eos_logical_ctrl(10)
     character(kind=c_char, len=1) :: eos_character_ctrl(10)

  end type c_EoS_General_Info

  contains

  !------------------------------------------------------------------------------
  ! Subroutines
  !------------------------------------------------------------------------------

  subroutine c_eos_ptr_2_f_eos_ptr(c_rq_ptr, frq, ierr)
    use eos_def, only: EoS_General_Info
    implicit none

    type(c_ptr), value :: c_rq_ptr
    type(c_EoS_General_Info), pointer :: rq
    type(EoS_General_Info), pointer :: frq
    integer(c_int), intent(out) :: ierr

    call c_f_pointer(c_rq_ptr, rq)
    if (.not. associated(rq)) then
      print *, "Error (libmesac:c_eos_interface_def). Pointer association failed."
      ierr = 1
      return
    end if

    frq%Z_all_HELM = rq%Z_all_HELM
    frq%logT_all_HELM = rq%logT_all_HELM
    frq%logT_low_all_HELM = rq%logT_low_all_HELM
    frq%coulomb_temp_cut_HELM = rq%coulomb_temp_cut_HELM
    frq%coulomb_den_cut_HELM = rq%coulomb_den_cut_HELM

    frq%use_OPAL_SCVH = rq%use_OPAL_SCVH
    frq%logT_low_all_SCVH = rq%logT_low_all_SCVH
    frq%logT_all_OPAL = rq%logT_all_OPAL
    frq%logRho1_OPAL_SCVH_limit = rq%logRho1_OPAL_SCVH_limit
    frq%logRho2_OPAL_SCVH_limit = rq%logRho2_OPAL_SCVH_limit
    frq%logRho_min_OPAL_SCVH_limit = rq%logRho_min_OPAL_SCVH_limit
    frq%logQ_max_OPAL_SCVH = rq%logQ_max_OPAL_SCVH
    frq%logQ_min_OPAL_SCVH = rq%logQ_min_OPAL_SCVH
    frq%Z_all_OPAL = rq%Z_all_OPAL

    frq%use_FreeEOS = rq%use_FreeEOS
    frq%logQ_max_FreeEOS_hi = rq%logQ_max_FreeEOS_hi
    frq%logQ_max_FreeEOS_lo = rq%logQ_max_FreeEOS_lo
    frq%logQ_min_FreeEOS_hi = rq%logQ_min_FreeEOS_hi
    frq%logQ_min_FreeEOS_lo = rq%logQ_min_FreeEOS_lo
    frq%logRho_min_FreeEOS_hi = rq%logRho_min_FreeEOS_hi
    frq%logRho_min_FreeEOS_lo = rq%logRho_min_FreeEOS_lo
    frq%logRho_max_FreeEOS_hi = rq%logRho_max_FreeEOS_hi
    frq%logRho_max_FreeEOS_lo = rq%logRho_max_FreeEOS_lo
    frq%logT_min_FreeEOS_hi = rq%logT_min_FreeEOS_hi
    frq%logT_min_FreeEOS_lo = rq%logT_min_FreeEOS_lo
    frq%logT_max_FreeEOS_hi = rq%logT_max_FreeEOS_hi
    frq%logT_max_FreeEOS_lo = rq%logT_max_FreeEOS_lo
    frq%logQ_cut_FreeEOS_lo_Z_max = rq%logQ_cut_FreeEOS_lo_Z_max
    frq%logQ_cut_lo_Z_FreeEOS_hi = rq%logQ_cut_lo_Z_FreeEOS_hi
    frq%logQ_cut_lo_Z_FreeEOS_lo = rq%logQ_cut_lo_Z_FreeEOS_lo
    frq%logQ_cut_hi_Z_FreeEOS_hi = rq%logQ_cut_hi_Z_FreeEOS_hi
    frq%logQ_cut_hi_Z_FreeEOS_lo = rq%logQ_cut_hi_Z_FreeEOS_lo
    frq%logRho_cut_FreeEOS_hi = rq%logRho_cut_FreeEOS_hi
    frq%logRho_cut_FreeEOS_lo = rq%logRho_cut_FreeEOS_lo
    frq%logT_cut_FreeEOS_hi = rq%logT_cut_FreeEOS_hi
    frq%logT_cut_FreeEOS_lo = rq%logT_cut_FreeEOS_lo
    frq%suffix_for_FreeEOS_Z = rq%suffix_for_FreeEOS_Z

    frq%use_CMS = rq%use_CMS
    frq%CMS_use_fixed_composition = rq%CMS_use_fixed_composition
    frq%CMS_fixed_composition_index = rq%CMS_fixed_composition_index
    frq%max_Z_for_any_CMS = rq%max_Z_for_any_CMS
    frq%max_Z_for_all_CMS = rq%max_Z_for_all_CMS
    frq%logQ_max_for_any_CMS = rq%logQ_max_for_any_CMS
    frq%logQ_max_for_all_CMS = rq%logQ_max_for_all_CMS
    frq%logQ_min_for_any_CMS = rq%logQ_min_for_any_CMS
    frq%logQ_min_for_all_CMS = rq%logQ_min_for_all_CMS
    frq%logRho_max_for_all_CMS = rq%logRho_max_for_all_CMS
    frq%logRho_max_for_any_CMS = rq%logRho_max_for_any_CMS
    frq%logRho_min_for_all_CMS = rq%logRho_min_for_all_CMS
    frq%logRho_min_for_any_CMS = rq%logRho_min_for_any_CMS
    frq%logT_max_for_all_CMS = rq%logT_max_for_all_CMS
    frq%logT_max_for_any_CMS = rq%logT_max_for_any_CMS
    frq%logT_min_for_all_CMS = rq%logT_min_for_all_CMS
    frq%logT_min_for_any_CMS = rq%logT_min_for_any_CMS
    frq%logT_max_for_all_CMS_pure_He = rq%logT_max_for_all_CMS_pure_He
    frq%logT_max_for_any_CMS_pure_He = rq%logT_max_for_any_CMS_pure_He

    frq%use_PC = rq%use_PC
    frq%mass_fraction_limit_for_PC = rq%mass_fraction_limit_for_PC
    frq%logRho1_PC_limit = rq%logRho1_PC_limit
    frq%logRho2_PC_limit = rq%logRho2_PC_limit
    frq%PC_use_Gamma_limit_instead_of_T = rq%PC_use_Gamma_limit_instead_of_T
    frq%logT1_PC_limit = rq%logT1_PC_limit
    frq%logT2_PC_limit = rq%logT2_PC_limit
    frq%log_Gamma_e_all_HELM = rq%log_Gamma_e_all_HELM
    frq%Gamma_e_all_HELM = rq%Gamma_e_all_HELM
    frq%log_Gamma_e_all_PC = rq%log_Gamma_e_all_PC
    frq%tiny_fuzz = rq%tiny_fuzz

    frq%PC_Gamma_start_crystal = rq%PC_Gamma_start_crystal
    frq%PC_Gamma_full_crystal = rq%PC_Gamma_full_crystal

    frq%use_Skye = rq%use_Skye
    frq%Skye_use_ion_offsets = rq%Skye_use_ion_offsets
    frq%mass_fraction_limit_for_Skye = rq%mass_fraction_limit_for_Skye
    frq%Skye_min_gamma_for_solid = rq%Skye_min_gamma_for_solid
    frq%Skye_max_gamma_for_liquid = rq%Skye_max_gamma_for_liquid
    frq%Skye_solid_mixing_rule = rq%Skye_solid_mixing_rule

    frq%use_simple_Skye_blends = rq%use_simple_Skye_blends
    frq%logRho_min_for_any_Skye = rq%logRho_min_for_any_Skye
    frq%logRho_min_for_all_Skye = rq%logRho_min_for_all_Skye
    frq%logT_min_for_any_Skye = rq%logT_min_for_any_Skye
    frq%logT_min_for_all_Skye = rq%logT_min_for_all_Skye

    frq%include_radiation = rq%include_radiation
    frq%include_elec_pos = rq%include_elec_pos
    frq%eosDT_use_linear_interp_for_X = rq%eosDT_use_linear_interp_for_X
    frq%eosDT_use_linear_interp_to_HELM = rq%eosDT_use_linear_interp_to_HELM
    
    frq%eosDT_file_prefix = rq%eosDT_file_prefix

    frq%okay_to_convert_ierr_to_skip = rq%okay_to_convert_ierr_to_skip

    frq%use_other_eos_component = rq%use_other_eos_component
    frq%use_other_eos_results = rq%use_other_eos_results

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

    frq%eos_ctrl = rq%eos_ctrl
    frq%eos_integer_ctrl = rq%eos_integer_ctrl
    frq%eos_logical_ctrl = rq%eos_logical_ctrl
    frq%eos_character_ctrl = rq%eos_character_ctrl
  end subroutine c_eos_ptr_2_f_eos_ptr

  subroutine f_eos_ptr_2_c_eos_ptr(c_rq_ptr, frq, ierr)
    use eos_def, only: EoS_General_Info
    implicit none

    type(c_ptr), value :: c_rq_ptr
    type(c_EoS_General_Info), pointer :: rq
    type(EoS_General_Info), pointer :: frq
    integer(c_int), intent(out) :: ierr

    call c_f_pointer(c_rq_ptr, rq)
    if (.not. associated(rq)) then
      print *, "Error (libmesac:c_eos_interface_def). Pointer association failed."
      ierr = 1
      return
    end if

    rq%Z_all_HELM = frq%Z_all_HELM
    rq%logT_all_HELM = frq%logT_all_HELM
    rq%logT_low_all_HELM = frq%logT_low_all_HELM
    rq%coulomb_temp_cut_HELM = frq%coulomb_temp_cut_HELM
    rq%coulomb_den_cut_HELM = frq%coulomb_den_cut_HELM

    rq%use_OPAL_SCVH = frq%use_OPAL_SCVH
    rq%logT_low_all_SCVH = frq%logT_low_all_SCVH
    rq%logT_all_OPAL = frq%logT_all_OPAL
    rq%logRho1_OPAL_SCVH_limit = frq%logRho1_OPAL_SCVH_limit
    rq%logRho2_OPAL_SCVH_limit = frq%logRho2_OPAL_SCVH_limit
    rq%logRho_min_OPAL_SCVH_limit = frq%logRho_min_OPAL_SCVH_limit
    rq%logQ_max_OPAL_SCVH = frq%logQ_max_OPAL_SCVH
    rq%logQ_min_OPAL_SCVH = frq%logQ_min_OPAL_SCVH
    rq%Z_all_OPAL = frq%Z_all_OPAL

    rq%use_FreeEOS = frq%use_FreeEOS
    rq%logQ_max_FreeEOS_hi = frq%logQ_max_FreeEOS_hi
    rq%logQ_max_FreeEOS_lo = frq%logQ_max_FreeEOS_lo
    rq%logQ_min_FreeEOS_hi = frq%logQ_min_FreeEOS_hi
    rq%logQ_min_FreeEOS_lo = frq%logQ_min_FreeEOS_lo
    rq%logRho_min_FreeEOS_hi = frq%logRho_min_FreeEOS_hi
    rq%logRho_min_FreeEOS_lo = frq%logRho_min_FreeEOS_lo
    rq%logRho_max_FreeEOS_hi = frq%logRho_max_FreeEOS_hi
    rq%logRho_max_FreeEOS_lo = frq%logRho_max_FreeEOS_lo
    rq%logT_min_FreeEOS_hi = frq%logT_min_FreeEOS_hi
    rq%logT_min_FreeEOS_lo = frq%logT_min_FreeEOS_lo
    rq%logT_max_FreeEOS_hi = frq%logT_max_FreeEOS_hi
    rq%logT_max_FreeEOS_lo = frq%logT_max_FreeEOS_lo
    rq%logQ_cut_FreeEOS_lo_Z_max = frq%logQ_cut_FreeEOS_lo_Z_max
    rq%logQ_cut_lo_Z_FreeEOS_hi = frq%logQ_cut_lo_Z_FreeEOS_hi
    rq%logQ_cut_lo_Z_FreeEOS_lo = frq%logQ_cut_lo_Z_FreeEOS_lo
    rq%logQ_cut_hi_Z_FreeEOS_hi = frq%logQ_cut_hi_Z_FreeEOS_hi
    rq%logQ_cut_hi_Z_FreeEOS_lo = frq%logQ_cut_hi_Z_FreeEOS_lo
    rq%logRho_cut_FreeEOS_hi = frq%logRho_cut_FreeEOS_hi
    rq%logRho_cut_FreeEOS_lo = frq%logRho_cut_FreeEOS_lo
    rq%logT_cut_FreeEOS_hi = frq%logT_cut_FreeEOS_hi
    rq%logT_cut_FreeEOS_lo = frq%logT_cut_FreeEOS_lo
    rq%suffix_for_FreeEOS_Z = frq%suffix_for_FreeEOS_Z

    rq%use_CMS = frq%use_CMS
    rq%CMS_use_fixed_composition = frq%CMS_use_fixed_composition
    rq%CMS_fixed_composition_index = frq%CMS_fixed_composition_index
    rq%max_Z_for_any_CMS = frq%max_Z_for_any_CMS
    rq%max_Z_for_all_CMS = frq%max_Z_for_all_CMS
    rq%logQ_max_for_any_CMS = frq%logQ_max_for_any_CMS
    rq%logQ_max_for_all_CMS = frq%logQ_max_for_all_CMS
    rq%logQ_min_for_any_CMS = frq%logQ_min_for_any_CMS
    rq%logQ_min_for_all_CMS = frq%logQ_min_for_all_CMS
    rq%logRho_max_for_all_CMS = frq%logRho_max_for_all_CMS
    rq%logRho_max_for_any_CMS = frq%logRho_max_for_any_CMS
    rq%logRho_min_for_all_CMS = frq%logRho_min_for_all_CMS
    rq%logRho_min_for_any_CMS = frq%logRho_min_for_any_CMS
    rq%logT_max_for_all_CMS = frq%logT_max_for_all_CMS
    rq%logT_max_for_any_CMS = frq%logT_max_for_any_CMS
    rq%logT_min_for_all_CMS = frq%logT_min_for_all_CMS
    rq%logT_min_for_any_CMS = frq%logT_min_for_any_CMS
    rq%logT_max_for_all_CMS_pure_He = frq%logT_max_for_all_CMS_pure_He
    rq%logT_max_for_any_CMS_pure_He = frq%logT_max_for_any_CMS_pure_He

    rq%use_PC = frq%use_PC
    rq%mass_fraction_limit_for_PC = frq%mass_fraction_limit_for_PC
    rq%logRho1_PC_limit = frq%logRho1_PC_limit
    rq%logRho2_PC_limit = frq%logRho2_PC_limit
    rq%PC_use_Gamma_limit_instead_of_T = frq%PC_use_Gamma_limit_instead_of_T
    rq%logT1_PC_limit = frq%logT1_PC_limit
    rq%logT2_PC_limit = frq%logT2_PC_limit
    rq%log_Gamma_e_all_HELM = frq%log_Gamma_e_all_HELM
    rq%Gamma_e_all_HELM = frq%Gamma_e_all_HELM
    rq%log_Gamma_e_all_PC = frq%log_Gamma_e_all_PC
    rq%tiny_fuzz = frq%tiny_fuzz

    rq%PC_Gamma_start_crystal = frq%PC_Gamma_start_crystal
    rq%PC_Gamma_full_crystal = frq%PC_Gamma_full_crystal

    rq%use_Skye = frq%use_Skye
    rq%Skye_use_ion_offsets = frq%Skye_use_ion_offsets
    rq%mass_fraction_limit_for_Skye = frq%mass_fraction_limit_for_Skye
    rq%Skye_min_gamma_for_solid = frq%Skye_min_gamma_for_solid
    rq%Skye_max_gamma_for_liquid = frq%Skye_max_gamma_for_liquid
    rq%Skye_solid_mixing_rule = frq%Skye_solid_mixing_rule

    rq%use_simple_Skye_blends = frq%use_simple_Skye_blends
    rq%logRho_min_for_any_Skye = frq%logRho_min_for_any_Skye
    rq%logRho_min_for_all_Skye = frq%logRho_min_for_all_Skye
    rq%logT_min_for_any_Skye = frq%logT_min_for_any_Skye
    rq%logT_min_for_all_Skye = frq%logT_min_for_all_Skye

    rq%include_radiation = frq%include_radiation
    rq%include_elec_pos = frq%include_elec_pos
    rq%eosDT_use_linear_interp_for_X = frq%eosDT_use_linear_interp_for_X
    rq%eosDT_use_linear_interp_to_HELM = frq%eosDT_use_linear_interp_to_HELM
    
    rq%eosDT_file_prefix = frq%eosDT_file_prefix

    rq%okay_to_convert_ierr_to_skip = frq%okay_to_convert_ierr_to_skip

    rq%use_other_eos_component = frq%use_other_eos_component
    rq%use_other_eos_results = frq%use_other_eos_results

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

    rq%eos_ctrl = frq%eos_ctrl
    rq%eos_integer_ctrl = frq%eos_integer_ctrl
    rq%eos_logical_ctrl = frq%eos_logical_ctrl
    rq%eos_character_ctrl = frq%eos_character_ctrl
  end subroutine f_eos_ptr_2_c_eos_ptr
end module eos_interface_def