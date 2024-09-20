!-----------------------------------------------------------------------
! Subroutine: c_eos_init
! Description: Initializes the eos library.
! Arguments:
!   - use_cache (logical, intent(in)): Flag to indicate whether to use cache.
!   - eos_cache_dir (character(kind=c_char, len=1), intent(in)): Directory for cache.
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_eos_init(use_cache, eos_cache_dir, ierr) bind(C, name="c_eos_init")
  use iso_c_binding, only: c_int, c_char
  use eos_lib, only: eos_init

  implicit none
  logical, intent(in) :: use_cache
  character(kind=c_char, len=1), intent(in) :: eos_cache_dir
  integer(c_int), intent(out) :: ierr

  call eos_init(eos_cache_dir, use_cache, ierr)
end subroutine c_eos_init

!-----------------------------------------------------------------------
! Subroutine: c_eos_shutdown
! Description: Shuts down the eos library.
!-----------------------------------------------------------------------
subroutine c_eos_shutdown() bind(C, name="c_eos_shutdown")
  use eos_lib, only: eos_shutdown
  implicit none

  call eos_shutdown()
end subroutine c_eos_shutdown

!-----------------------------------------------------------------------
! Function: c_alloc_eos_handle
! Description: Allocates a handle for eos operations.
! Arguments:
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
! Returns:
!   - handle (integer): Allocated handle.
!-----------------------------------------------------------------------
integer function c_alloc_eos_handle(ierr) result(haldle) bind(C, name="c_alloc_eos_handle")
  use eos_lib, only: alloc_eos_handle
  use iso_c_binding, only: c_int
  
  implicit none
  integer(c_int), intent(out) :: ierr

  haldle = alloc_eos_handle(ierr)
end function c_alloc_eos_handle

!-----------------------------------------------------------------------
! Function: c_alloc_eos_handle_using_inlist
! Description: Allocates a handle for eos operations using an input list.
! Arguments:
!   - inlist (character(kind=c_char, len=*), intent(in)): Input list.
!   - inlist_len (integer(c_int), intent(in)): Length of the input list.
!   - ierr (integer(c_int), intent(out)): Error code (0 means success).
! Returns:
!   - handle (integer): Allocated handle.
!-----------------------------------------------------------------------
integer function c_alloc_eos_handle_using_inlist(inlist, inlist_len, ierr) &
  result(haldle) bind(C, name="c_alloc_eos_handle_using_inlist")
  use eos_lib, only: alloc_eos_handle_using_inlist
  use iso_c_binding, only: c_int, c_char

  implicit none
  character(kind=c_char, len=*), intent(in) :: inlist
  integer(c_int), intent(in) :: inlist_len
  integer(c_int), intent(out) :: ierr

  haldle = alloc_eos_handle_using_inlist(inlist(1:inlist_len), ierr)
end function c_alloc_eos_handle_using_inlist

!-----------------------------------------------------------------------
! Subroutine: c_free_eos_handle
! Description: Frees the eos handle.
! Arguments:
!   - handle (integer, intent(in)): Handle to free.
!-----------------------------------------------------------------------
subroutine c_free_eos_handle(handle) bind(C, name="c_free_eos_handle")
  use eos_lib, only: free_eos_handle

  implicit none
  integer, intent(in) :: handle

  call free_eos_handle(handle)
end subroutine c_free_eos_handle

!-----------------------------------------------------------------------
! Subroutine: c_eos_ptr
! Description: Gets the pointer to the eos handle.
! Arguments:
!  - handle (integer, intent(in)): Handle to get pointer for.
!  - rq (c_EoS_General_info, intent(out)): Pointer to the handle.
!  - ierr (integer(c_int), intent(out)): Error code (0 means success).
!-----------------------------------------------------------------------
subroutine c_eos_ptr(handle, rq, ierr) bind(C, name="c_eos_ptr")
  use eos_lib, only: eos_ptr
  use eos_def, only: EoS_General_info
  use eos_interface_def, only: c_EoS_General_info, f_eos_ptr_2_c_eos_ptr
  use iso_c_binding, only: c_int, c_ptr

  implicit none
  integer, intent(in) :: handle
  type(c_ptr), value :: rq
  type(EoS_General_info), pointer :: frq
  integer(c_int), intent(out) :: ierr

  call eos_ptr(handle, frq, ierr)
  call f_eos_ptr_2_c_eos_ptr(rq, frq, ierr)
end subroutine 

subroutine c_eosDT_get(handle, species, chem_id, net_iso, xa, &
                      Rho, logRho, T, logT, &
                      res, d_dlnd, d_dlnT, d_dxa, ierr) &
                      bind(C, name="c_eosDT_get")

  use eos_lib, only: eosDT_get
  use, intrinsic :: iso_fortran_env, dp => real64

  implicit none
  integer, intent(in) :: handle ! eos handle; from star, pass s% eos_handle
  integer, intent(in) :: species ! number of species
  integer, pointer :: chem_id(:) ! maps species to chem id
  integer, pointer :: net_iso(:) ! maps chem id to species number
  real(dp), intent(in) :: xa(:) ! mass fractions         
  real(dp), intent(in) :: Rho, logRho ! the density
  real(dp), intent(in) :: T, logT ! the temperature         
  real(dp), intent(inout) :: res(:) ! (num_eos_basic_results)         
  real(dp), intent(inout) :: d_dlnd(:) ! (num_eos_basic_results) 
  real(dp), intent(inout) :: d_dlnT(:) ! (num_eos_basic_results)
  real(dp), intent(inout) :: d_dxa(:,:) ! (num_eos_d_dxa_results,species)
  integer, intent(out) :: ierr ! 0 means AOK.

  call eosDT_get(handle, species, chem_id, net_iso, xa, &
                 Rho, logRho, T, logT, &
                 res, d_dlnd, d_dlnT, d_dxa, ierr)

end subroutine c_eosDT_get

