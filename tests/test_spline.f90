program test_spline
  use simsphere_mod
  implicit none

  integer, parameter :: max_array = 62
  integer :: i
  real :: arg1(max_array), arg2(max_array), arg4, arg5 
  real :: output(max_array)

!  if (.not. allocated(output)) then
!    allocate(output(max_array))
!    output = 0.0
!  end if

  output = 0.0
  do i = 1,max_array
    arg1 = 2*i
    arg2 = 2*i
  end do
  arg1 = 1.0
  arg2 = 2.0
  arg4 = 1.0
  arg5 = 2.5

  output = spline(arg1, arg2, max_array, arg4, arg5)

  do i = 1,max_array
    write(*,*) output(i)
  end do

!  if (allocated(output)) then
!    deallocate(output)
!  end if


end program
