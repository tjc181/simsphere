subroutine avr(T_Unsmoothed, T_smoothed, init)
  implicit none

  real, save :: av_array(4)=0.0
  real :: sum_array=0.0, T_Unsmoothed, T_smoothed
  integer :: init, i=0, j=0, k=0

  if (init == 1) then ! fill all 4 elements with initial value of otemp

    do i = 1,4
      av_array(i) = T_Unsmoothed
    end do
  
    init = 2

  else

    do j = 2,4
      av_array(j-1) = av_array(j)
    end do

    av_array(4) = T_Unsmoothed

  endif

  sum_array = 0
  do k = 1,4
    sum_array = av_array(k) + sum_array
  end do

  T_smoothed = sum_array / 4

  return
end
