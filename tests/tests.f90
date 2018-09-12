program test_simsphere
  use simsphere_mod
  implicit none


  integer :: i

  real(kind=4) :: T_Obst_Hgt, T_zo_patch
  logical :: T_dual_regime
  
  logical :: splint_test, spline_test, start_test

! splint_test variables
  real :: st_arg1(50), st_arg2(50)  
  real(kind=4), allocatable :: st_arg3(:)
  integer :: st_arg4, st_arg5
  real :: st_arg6

! Set logical to control test execution
  start_test = .false.
  splint_test = .true.
  spline_test = .false.


! Initialize some test values
  T_Obst_Hgt = 0.0
  T_zo_patch = 0.0
  T_dual_regime = .false.


!
! splint_test
!
  if (splint_test) then
    st_arg1 = 0.0
    st_arg2 = 1.0
    st_arg4 = 4
    if (.not. allocated(st_arg3)) then
      allocate(st_arg3(st_arg4))
      st_arg3 = 1.0
    end if
    st_arg5 = 3
    st_arg6 = 3.5

    call splint(st_arg1, st_arg2, st_arg3, st_arg4, st_arg5, st_arg6)
    write(*,*) st_arg1
    write(*,*) st_arg2
    write(*,*) st_arg3
    write(*,*) st_arg4
    write(*,*) st_arg5
    write(*,*) st_arg6
  end if

!
! ntoken test
!
!  if (ntoken_test) then
!    output = ntoken(input_str,input_delim)
!
!    if (output .ne. ntoken_result) then
!      write(*,*) 'ntoken_test: left /= right: ', output, ntoken_result
!      stop 
!    else
!      write(*,*) 'ntoken: OK'
!    end if
!  end if

end program
