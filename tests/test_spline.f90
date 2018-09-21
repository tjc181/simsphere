program test_spline
  use simsphere_mod
  implicit none

  integer, parameter :: max_array = 50
  integer :: i
  real :: arg1(max_array), arg2(max_array), arg4, arg5 
  real, allocatable :: output(:)

  if (.not. allocated(output)) then
    allocate(output(max_array))
  end if

  do i = 1,max_array
    arg1 = 2*i
    arg2 = 2*i
  end do
  arg4 = 1.0
  arg5 = 2.5

  output = spline(arg1, arg2, max_array, arg4, arg5)

  do i = 1,max_array
    write(*,*) output(i)
  end do

  if (allocated(output)) then
    deallocate(output)
  end if


end program
