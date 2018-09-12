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
  integer, parameter :: splint_max_array = 50
  real :: splint_output

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
    do i = 1,splint_max_array
      st_arg1(i) = 2*i
      st_arg2(i) = 2*i
    end do
    st_arg4 = 12
    if (.not. allocated(st_arg3)) then
      allocate(st_arg3(st_arg4))
      do i = 1,st_arg4
        st_arg3(i) = 2*i
      end do
    end if
    st_arg5 = 3

    splint_output=splint(st_arg1, st_arg2, st_arg3, st_arg4, st_arg5)
    if (splint_output .ne. 1.5) then
      write(*,*) 'splint_test: left /= right: ', splint_output, 1.5
    else
      write(*,*) 'splint_test: OK'
    end if
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
