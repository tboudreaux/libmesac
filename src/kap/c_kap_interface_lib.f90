

!-----------------------------------------------------------------------
! Subroutine: c_kap_init
! Description: Initializes the KAP library.
! Arguments:
!   - use_cache (logical, intent(in)): Flag to indicate whether to use cache.
!   - kap_cache_dir (character(kind=c_char, len=1), intent(in)): Directory for cache.
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_kap_init(use_cache, kap_cache_dir, ierr) bind(C, name="c_kap_init")
  use iso_c_binding, only: c_int, c_char
  use kap_lib, only: kap_init

  implicit none
  logical, intent(in) :: use_cache
  character(kind=c_char, len=1), intent(in) :: kap_cache_dir
  integer(c_int), intent(out) :: ierr

  call kap_init(use_cache, kap_cache_dir, ierr)
end subroutine c_kap_init

!-----------------------------------------------------------------------
! Subroutine: c_kap_shutdown
! Description: Shuts down the KAP library.
!-----------------------------------------------------------------------
subroutine c_kap_shutdown() bind(C, name="c_kap_shutdown")
  use kap_def, only: kap_is_initialized
  use kap_lib, only: kap_shutdown
  
  implicit none

  if(kap_is_initialized) then
    print *, "kap is initialized"
    call kap_shutdown()
  else
    print *, "kap is not initialized"
  end if
end subroutine c_kap_shutdown


!-----------------------------------------------------------------------
! Function: c_alloc_kap_handle
! Description: Allocates a handle for KAP operations.
! Arguments:
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
! Returns:
!   - handle (integer): Allocated handle.
!-----------------------------------------------------------------------
integer function c_alloc_kap_handle(ierr) result(haldle) bind(C, name="c_alloc_kap_handle")
  use kap_lib, only: alloc_kap_handle
  use iso_c_binding, only: c_int
  
  implicit none
  integer(c_int), intent(out) :: ierr

  haldle = alloc_kap_handle(ierr)
end function c_alloc_kap_handle

!-----------------------------------------------------------------------
! Function: c_alloc_kap_handle_using_inlist
! Description: Allocates a handle for KAP operations using an input list.
! Arguments:
!   - inlist (character(kind=c_char, len=*), intent(in)): Input list.
!   - inlist_len (integer(c_int), intent(in)): Length of the input list.
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
! Returns:
!   - handle (integer): Allocated handle.
!-----------------------------------------------------------------------
integer function c_alloc_kap_handle_using_inlist(inlist, inlist_len, ierr) &
  result(haldle) bind(C, name="c_alloc_kap_handle_using_inlist")
  use kap_lib, only: alloc_kap_handle_using_inlist
  use iso_c_binding, only: c_int, c_char

  implicit none
  character(kind=c_char, len=*), intent(in) :: inlist
  integer(c_int), intent(in) :: inlist_len
  integer(c_int), intent(out) :: ierr

  haldle = alloc_kap_handle_using_inlist(inlist(1:inlist_len), ierr)
end function c_alloc_kap_handle_using_inlist

!-----------------------------------------------------------------------
! Subroutine: c_mesa_microphysics_init
! Description: Initializes the MESA microphysics.
! Arguments:
!   - mesa_dir_c (character(kind=c_char, len=*), intent(in)): MESA directory.
!   - isotopes_c (character(kind=c_char, len=*), intent(in)): Isotopes.
!   - use_cache (logical, intent(in)): Flag to indicate whether to use cache.
!-----------------------------------------------------------------------
subroutine c_mesa_kap_full_init(mesa_dir_c, isotopes_c, use_cache, &
  kap_cache_dir_c, ierr, mesa_dir_length, isotopes_length, &
  kap_cache_dir_length) bind(C, name="c_mesa_kap_full_init")

  use chem_lib, only: chem_init
  use math_lib, only: math_init
  use const_lib, only: const_init
  use kap_lib, only: kap_init

  use iso_c_binding, only: c_int, c_char

  implicit none
  character(kind=c_char, len=*), intent(in) :: mesa_dir_c
  character(kind=c_char, len=*), intent(in) :: isotopes_c
  character(kind=c_char, len=*), intent(in) :: kap_cache_dir_c
  logical, intent(in) :: use_cache
  integer(c_int), intent(in) :: mesa_dir_length
  integer(c_int), intent(in) :: isotopes_length
  integer(c_int), intent(in) :: kap_cache_dir_length
  integer(c_int), intent(out) :: ierr

  character(len=256) :: mesa_dir
  character(len=256) :: isotopes
  character(len=256) :: kap_cache_dir

  mesa_dir = mesa_dir_c(1:mesa_dir_length)
  isotopes = isotopes_c(1:isotopes_length)
  kap_cache_dir = kap_cache_dir_c(1:kap_cache_dir_length)


  call const_init(mesa_dir, ierr)
  call math_init()
  call chem_init(isotopes, ierr) 
 
  call kap_init(use_cache, kap_cache_dir, ierr)
end subroutine c_mesa_kap_full_init

!-----------------------------------------------------------------------
! Subroutine: c_free_kap_handle
! Description: Frees the KAP handle.
! Arguments:
!   - handle (integer, intent(in)): Handle to free.
!-----------------------------------------------------------------------
subroutine c_free_kap_handle(handle) bind(C, name="c_free_kap_handle")
  use kap_lib, only: free_kap_handle

  implicit none
  integer, intent(in) :: handle

  call free_kap_handle(handle)
end subroutine c_free_kap_handle

!-----------------------------------------------------------------------
! Subroutine: c_kap_ptr
! Description: Gets the KAP pointer.
! Arguments:
!   - handle (integer, intent(in)): Handle.
!   - c_rq_ptr (type(c_ptr), value): Pointer to the KAP general info.
!   - ierr (integer, intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_kap_ptr(handle, c_rq_ptr, ierr) bind(C, name="c_kap_ptr")
  use kap_lib, only: kap_ptr
  use kap_def, only: Kap_General_Info, get_kap_ptr, kap_is_initialized
  use kap_interface_def, only: c_Kap_General_Info, f_kap_ptr_2_c_kap_ptr
  use iso_c_binding, only: c_int, c_double, c_ptr, c_f_pointer

  implicit none
  integer, intent(in) :: handle
  type (c_ptr), value :: c_rq_ptr
  integer, intent(out) :: ierr

  type(Kap_General_Info), pointer :: frq

  call kap_ptr(handle, frq, ierr)
  call f_kap_ptr_2_c_kap_ptr(c_rq_ptr, frq, ierr)

end subroutine c_kap_ptr

!-----------------------------------------------------------------------
! Subroutine: c_kap_setup_tables
! Description: Sets up the KAP tables.
! Arguments:
!   - handle (integer, intent(in)): Handle.
!   - ierr (integer, intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_kap_setup_tables(handle, ierr) bind(C, name='c_kap_setup_tables')
  use kap_lib, only: kap_setup_tables

  integer, intent(in) :: handle
  integer, intent(out) :: ierr

  call kap_setup_tables(handle, ierr)
end subroutine c_kap_setup_tables

!-----------------------------------------------------------------------
! Subroutine: c_kap_setup_hooks
! Description: Sets up the KAP hooks.
! Arguments:
!   - handle (integer, intent(in)): Handle.
!   - ierr (integer, intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_kap_setup_hooks(handle, ierr) bind(C, name="c_kap_setup_hooks")
  use kap_lib, only: kap_setup_hooks
  integer, intent(in) :: handle
  integer, intent(out) :: ierr
  call kap_setup_hooks(handle, ierr)
end subroutine c_kap_setup_hooks


!-----------------------------------------------------------------------
! Function: c_get_num_kap_fracs
! Description: Gets the number of KAP fractions.
! Returns:
!   - n (integer): Number of KAP fractions.
!-----------------------------------------------------------------------
integer function c_get_num_kap_fracs() result(n) bind(C, name="c_get_num_kap_fracs")
  use kap_def, only: num_kap_fracs
  n = num_kap_fracs
end function c_get_num_kap_fracs

!-----------------------------------------------------------------------
! Subroutine: c_kap_get
! Description: Gets the KAP opacity.
! Arguments:
!   - handle (integer, intent(in)): Handle.
!   - species (integer, intent(in)): Species.
!   - chem_id_ptr (type(c_ptr), value): Pointer to the chemical id array.
!   - net_iso_ptr (type(c_ptr), value): Pointer to the net isotope array.
!   - xa_ptr (type(c_ptr), value): Pointer to the mass fractions array.
!   - NSpec (integer, intent(in)): Number of species.
!   - maxpts (integer, intent(in)): Maximum number of points.
!   - logRho (real(dp), intent(in)): Log density.
!   - logT (real(dp), intent(in)): Log temperature.
!   - lnfree_e (real(dp), intent(in)): Free electrons.
!   - d_lnfree_e_dlnRho (real(dp), intent(in)): Partial derivative of free electrons w.r.t. density.
!   - d_lnfree_e_dlnT (real(dp), intent(in)): Partial derivative of free electrons w.r.t. temperature.
!   - eta (real(dp), intent(in)): Electron degeneracy parameter.
!   - d_eta_dlnRho (real(dp), intent(in)): Partial derivative of electron degeneracy parameter w.r.t. density.
!   - d_eta_dlnT (real(dp), intent(in)): Partial derivative of electron degeneracy parameter w.r.t. temperature.
!   - kap_fracs (real(dp), intent(out)): KAP fractions.
!   - cref_num_kap_fracs (integer, intent(out)): Number of KAP fractions.
!   - kap (real(dp), intent(out)): KAP opacity.
!   - dlnkap_dlnRho (real(dp), intent(out)): Partial derivative of KAP opacity w.r.t. density.
!   - dlnkap_dlnT (real(dp), intent(out)): Partial derivative of KAP opacity w.r.t. temperature.
!   - dlnkap_dxa (real(dp), intent(out)): Partial derivative of KAP opacity w.r.t. species.
!   - ierr (integer, intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_kap_get( &
  handle, species, chem_id_ptr, net_iso_ptr, xa_ptr, NSpec, maxpts, &
  logRho, logT, &
  lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
  eta, d_eta_dlnRho, d_eta_dlnT , &
  kap_fracs, cref_num_kap_fracs, kap, dlnkap_dlnRho, dlnkap_dlnT, dlnkap_dxa, ierr) &
  bind(C, name="c_kap_get")

  use kap_def, only : kap_is_initialized, Kap_General_Info, num_kap_fracs
  use kap_lib, only : kap_get
  use const_def, only: dp
  use chem_def
  use iso_c_binding, only: c_int, c_double, c_ptr, c_f_pointer

  implicit none
  ! Inputs
  integer, intent(in) :: handle 
  integer, intent(in) :: species
  type(c_ptr), value :: chem_id_ptr 
  integer(c_int), pointer :: chem_id(:)
  type(c_ptr), value :: net_iso_ptr 
  integer(c_int), pointer :: net_iso(:) 
  type(c_ptr), value :: xa_ptr
  real(dp), pointer :: xa(:)
  integer(c_int), intent(in) :: NSpec
  integer(c_int), intent(in) :: maxpts
  real(dp), intent(in) :: logRho 
  real(dp), intent(in) :: logT 
  real(dp), intent(in) :: lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT
  real(dp), intent(in)  :: eta, d_eta_dlnRho, d_eta_dlnT

  ! Outputs
  real(dp), intent(out) :: kap_fracs(num_kap_fracs)
  integer, intent(out) :: cref_num_kap_fracs
  real(dp), intent(out) :: kap 
  real(dp), intent(out) :: dlnkap_dlnRho 
  real(dp), intent(out) :: dlnkap_dlnT   
  real(dp), intent(out) :: dlnkap_dxa(NSpec) 
  integer, intent(out) :: ierr 

  ! Associate c pointers to fortran arrays
  type (Kap_General_Info), pointer :: rq
  call c_f_pointer(chem_id_ptr, chem_id, [NSpec])
  call c_f_pointer(net_iso_ptr, net_iso, [NSpec])
  call c_f_pointer(xa_ptr, xa, [NSpec])

  cref_num_kap_fracs = num_kap_fracs
  if (.not. associated(chem_id)) then
      print *, "Error (libmesac:c_kap_interface_lib.f90) chem_id is not associated."
      ierr = -1
      return
  endif
  if (.not. associated(net_iso)) then
      print *, "Error (libmesac:c_kap_interface_lib.f90) net_iso is not associated."
      ierr = -1
      return
  endif
  if (.not. associated(xa)) then
      print *, "Error (libmesac:c_kap_interface_lib.f90) xa is not associated."
      ierr = -1
      return
  endif

  call kap_get(handle, species, chem_id, net_iso, xa, &
              logRho, logT, &
              lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
              eta, d_eta_dlnRho, d_eta_dlnT, &
              kap_fracs, kap, dlnkap_dlnRho, dlnkap_dlnT, dlnkap_dxa, ierr)

end subroutine c_kap_get
    
