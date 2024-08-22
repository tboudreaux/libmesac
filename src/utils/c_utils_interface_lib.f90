subroutine c_simple_model_read(path_c, path_len, Mstar, Z_init, Npts, Nspec, &
  lnRho, lnT, lnR, L, dq, X, ierr) bind(C, name="c_simple_model_read")

  use const_def, only: dp
  use iso_c_binding, only: c_int, c_char, c_double

  implicit none
  integer, parameter :: maxpts=2000, maxspec=31, iounit=99
  integer, intent(in) :: path_len
  character(kind=c_char, len=*) :: path_c
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
