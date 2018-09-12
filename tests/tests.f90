program test_simsphere
  use simsphere_mod
  implicit none


  integer :: i
  real(kind=4) :: T_Obst_Hgt, T_zo_patch
  logical :: T_dual_regime
  
  logical :: splint_test, spline_test, start_test

! splint_test variables
  integer, parameter :: splint_max_array = 50
  real, parameter :: splint_expected = 1.5
  real :: splint_arg1(splint_max_array), splint_arg2(splint_max_array)  
  real(kind=4), allocatable :: splint_arg3(:)
  integer :: splint_arg4, splint_arg5
  real :: splint_output

! spline_test variables
  integer, parameter :: spline_max_array = 50
  integer :: spline_arg3
  real, parameter :: spline_expected = 2.59807634
  real :: spline_arg1(spline_max_array), spline_arg2(spline_max_array)
  real :: spline_arg4, spline_arg5
  real, allocatable :: spline_output(:)

! Set logical to control test execution
  start_test = .false.
  splint_test = .true.
  spline_test = .true.


! Initialize some test values
  T_Obst_Hgt = 0.0
  T_zo_patch = 0.0
  T_dual_regime = .false.


!
! splint_test
!
  if (splint_test) then
    do i = 1,splint_max_array
      splint_arg1(i) = 2*i
      splint_arg2(i) = 2*i
    end do
    splint_arg4 = 12
    if (.not. allocated(splint_arg3)) then
      allocate(splint_arg3(splint_arg4))
      do i = 1,splint_arg4
        splint_arg3(i) = 2*i
      end do
    end if
    splint_arg5 = 3

    splint_output=splint(splint_arg1, splint_arg2, splint_arg3, splint_arg4, splint_arg5)
    if (splint_output .ne. splint_expected) then
      write(*,*) 'splint_test: left /= right: ', splint_output, splint_expected
    else
      write(*,*) 'splint_test: OK'
    end if
    if (allocated(splint_arg3)) then
      deallocate(splint_arg3)
    end if
  end if

! 
! spline_test
!
  if (spline_test) then
    do i = 1,spline_max_array
      spline_arg1(i) = 2*i
      spline_arg2(i) = 2*i
    end do
    spline_arg3 = spline_max_array
    spline_arg4 = 1.0
    spline_arg5 = 2.5
    if (.not. allocated(spline_output)) then
      allocate(spline_output(spline_arg3))
    end if
    spline_output=spline(spline_arg1, spline_arg2, spline_arg3, spline_arg4, spline_arg5)
    if (spline_output(size(spline_output)) .ne. spline_expected) then
      write(*,*) 'spline_test: left /= right: ', spline_output, spline_expected
    else
      write(*,*) 'spline_test: OK'
    end if
    if (allocated(spline_output)) then
      deallocate(spline_output)
    end if
  end if

end program
