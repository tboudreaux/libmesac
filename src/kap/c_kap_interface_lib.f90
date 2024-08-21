

subroutine c_kap_init(use_cache, kap_cache_dir, ierr) bind(C, name="c_kap_init")
  use iso_c_binding, only: c_int, c_char
  use kap_lib, only: kap_init

  implicit none
  logical, intent(in) :: use_cache
  character(kind=c_char, len=1), intent(in) :: kap_cache_dir
  integer(c_int), intent(out) :: ierr

  call kap_init(use_cache, kap_cache_dir, ierr)
end subroutine c_kap_init

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

integer function c_alloc_kap_handle(ierr) result(haldle) bind(C, name="c_alloc_kap_handle")
  use kap_lib, only: alloc_kap_handle
  use iso_c_binding, only: c_int
  
  implicit none
  integer(c_int), intent(out) :: ierr

  haldle = alloc_kap_handle(ierr)
end function c_alloc_kap_handle


integer function c_alloc_kap_handle_using_inlist(inlist, inlist_len, ierr) &
  result(haldle) bind(C, name="c_alloc_kap_handle_using_inlist")
  use kap_lib, only: alloc_kap_handle_using_inlist
  use iso_c_binding, only: c_int, c_char

  implicit none
  character(kind=c_char, len=1), intent(in) :: inlist
  integer(c_int), intent(in) :: inlist_len
  integer(c_int), intent(out) :: ierr

  haldle = alloc_kap_handle_using_inlist(inlist(1:inlist_len), ierr)
end function c_alloc_kap_handle_using_inlist

subroutine c_mesa_microphysics_init(mesa_dir_c, isotopes_c, use_cache, &
  kap_cache_dir_c, ierr, mesa_dir_length, isotopes_length, &
  kap_cache_dir_length) bind(C, name="c_mesa_microphysics_init")

  use chem_lib, only: chem_init
  use math_lib, only: math_init
  use const_lib, only: const_init
  use kap_lib, only: kap_init

  use iso_c_binding, only: c_int, c_char

  implicit none
  character(kind=c_char, len=1), intent(in) :: mesa_dir_c
  character(kind=c_char, len=1), intent(in) :: isotopes_c
  character(kind=c_char, len=1), intent(in) :: kap_cache_dir_c
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
end subroutine c_mesa_microphysics_init

subroutine c_free_kap_handle(handle) bind(C, name="c_free_kap_handle")
  use kap_lib, only: free_kap_handle

  implicit none
  integer, intent(in) :: handle

  call free_kap_handle(handle)
end subroutine c_free_kap_handle


subroutine c_kap_ptr(handle, c_rq_ptr, ierr) bind(C, name="c_kap_ptr")
  use kap_lib, only: kap_ptr
  use kap_def, only: Kap_General_Info, get_kap_ptr, kap_is_initialized
  use interface_def, only: c_Kap_General_Info, f_kap_ptr_2_c_kap_ptr
  use iso_c_binding, only: c_int, c_double, c_ptr, c_f_pointer

  implicit none
  integer, intent(in) :: handle
  type (c_ptr), value :: c_rq_ptr
  integer, intent(out) :: ierr

  type(Kap_General_Info), pointer :: frq

  call kap_ptr(handle, frq, ierr)
  call f_kap_ptr_2_c_kap_ptr(c_rq_ptr, frq, ierr)

end subroutine c_kap_ptr

subroutine c_kap_setup_tables(handle, ierr) bind(C, name='c_kap_setup_tables')
  use kap_lib, only: kap_setup_tables

  integer, intent(in) :: handle
  integer, intent(out) :: ierr

  call kap_setup_tables(handle, ierr)
end subroutine c_kap_setup_tables

subroutine c_kap_setup_hooks(handle, ierr) bind(C, name="c_kap_setup_hooks")
  use kap_lib, only: kap_setup_hooks
  integer, intent(in) :: handle
  integer, intent(out) :: ierr
  call kap_setup_hooks(handle, ierr)
end subroutine c_kap_setup_hooks

! This should be in chem
subroutine c_get_some_isos(c_chem_ptr) bind(C, name="c_get_some_isos")
  use interface_def, only: c_set_some_isos, c_Chem_Ids
  use iso_c_binding, only: c_ptr, c_f_pointer
  type(c_ptr), value :: c_chem_ptr
  type(c_Chem_Ids), pointer :: chem_ids

  call c_f_pointer(c_chem_ptr, chem_ids)
  call c_set_some_isos(chem_ids)
end subroutine c_get_some_isos

integer function c_get_num_chem_isos() result(nci) bind(C, name="c_get_num_chem_isos")
  use chem_def, only: num_chem_isos

  nci = num_chem_isos
end function c_get_num_chem_isos
  

subroutine c_kap_get( &
  handle, species, chem_id, net_iso, xa, &
  logRho, logT, &
  lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
  eta, d_eta_dlnRho, d_eta_dlnT , &
  kap_fracs, cref_num_kap_fracs, kap, dlnkap_dlnRho, dlnkap_dlnT, dlnkap_dxa, ierr) &
  bind(C, name="c_kap_get")

  use kap_def, only : kap_is_initialized, Kap_General_Info, num_kap_fracs
  use kap_lib, only : kap_get
  use const_def, only: dp
  use chem_def
  use iso_c_binding, only: c_int, c_double, c_ptr

  integer, intent(in) :: handle ! from alloc_kap_handle; in star, pass s% kap_handle
  integer, intent(in) :: species
  integer, pointer :: chem_id(:) ! maps species to chem id
  integer, pointer :: net_iso(:) ! maps chem id to species number
  real(dp), intent(in) :: xa(:) ! mass fractions
  real(dp), intent(in) :: logRho ! density
  real(dp), intent(in) :: logT ! temperature
  real(dp), intent(in) :: lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT
  ! free_e := total combined number per nucleon of free electrons and positrons
  real(dp), intent(in)  :: eta, d_eta_dlnRho, d_eta_dlnT
  ! eta := electron degeneracy parameter from eos

  ! OUTPUT
  real(dp), intent(out) :: kap_fracs(num_kap_fracs)
  integer, intent(out) :: cref_num_kap_fracs
  real(dp), intent(out) :: kap ! opacity
  real(dp), intent(out) :: dlnkap_dlnRho ! partial derivative at constant T
  real(dp), intent(out) :: dlnkap_dlnT   ! partial derivative at constant Rho
  real(dp), intent(out) :: dlnkap_dxa(:) ! partial derivative w.r.t. species
  integer, intent(out) :: ierr ! 0 means AOK.

  type (Kap_General_Info), pointer :: rq

  cref_num_kap_fracs = num_kap_fracs

  ! call kap_get(handle, species, cham_id, net_iso, xa, &
  !             logRho, logT, &
  !             lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
  !             eta, d_eta_dlnRho, d_eta_dlnT, &
  !             kap_fracs, kap, dlnkap_dlnRho, dlnkap_dlnT, dlnkap_dxa, ierr)


end subroutine c_kap_get
    
subroutine c_simple_model_read(path_c, path_len, Mstar, Z_init, Npts, Nspec, &
  lnRho, lnT, lnR, L, dq, X, ierr) bind(C, name="c_simple_model_read")

  use const_def, only: dp
  use iso_c_binding, only: c_int, c_char, c_double

  implicit none
  integer, parameter :: maxpts=2000, maxspec=31, iounit=99
  integer, intent(in) :: path_len
  character(kind=c_char, len=1) :: path_c
  character(len=256) :: path
  real(c_double), intent(out) :: Mstar, Z_init
  integer(c_int), intent(out) :: Npts, Nspec
  real(dp), intent(out) :: lnRho(maxpts), lnT(maxpts), X(maxspec,maxpts)
  real(dp), intent(out) :: lnR(maxpts), L, dq(maxpts)
  integer :: ii, i


  integer, intent(out) :: ierr

  path = path_c(1:path_len)

  open(unit=iounit,file=trim(path),status='old',iostat=ierr)
  read(iounit,*)
  read(iounit,*)            !skip 3 header lines
  read(iounit,*)
  read(iounit,1) Mstar      !read stellar mass
  read(iounit,1) Z_init     !read initial Z
  read(iounit,2) Npts       !read number of points in model
  read(iounit,*)            !skip
  read(iounit,2) Nspec      !read number of chemical species in model
  read(iounit,*)            !skip 2 lines
  read(iounit,*)
  
  
  do i=1,Npts               !read model
     read(iounit,*) ii, lnRho(i), lnT(i), lnR(i), L, dq(i), X(1:Nspec,i)
     if (ii /= i) then
        write(*,*) 'bad data for zone', i
        stop
     end if
  enddo
  close(iounit)

 1    format(37x,e23.16)
 2    format(37x,i6)
 3    format(a28,99(a26,1x))
 4    format(i28,99(1pes26.16e3,1x))
end subroutine c_simple_model_read
