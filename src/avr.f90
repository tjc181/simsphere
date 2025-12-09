subroutine avr(T_Unsmoothed, T_smoothed, av_array)
  implicit none

  real, intent(inout) :: av_array(4)
  real, intent(in) :: T_Unsmoothed 
  real, intent(out) :: T_smoothed
  real :: sum_array
  integer :: i, j, k

  do j = 2,4
    av_array(j-1) = av_array(j)
  end do

  av_array(4) = T_Unsmoothed

  sum_array = 0
  do k = 1,4
    sum_array = av_array(k) + sum_array
  end do

  T_smoothed = sum_array / 4

  return
end
