subroutine  PRFILE
  use simsphere_mod

! This routine generates the daytime and night-time vertical wind
! profiles of the geostrophic wind components at intervals of 250m
! from 50 m (top of sfc layer), from the surface geostrophic winds.
! Calculated in 3 different ways; specified by user.


!      INCLUDE 'modvars.h'


  j = 1
  u_fine(1) = ud(1)
  v_fine(1) = vd(1)
  t_fine(1) = td(1)
  q_fine(1) = qd(1)
        
  do i = 1,10
    do incr = 1,5
      j = j +1  
      u_fine(j) = rlinear(ud(i+1),ud(i),incr) 
      v_fine(j) = rlinear(vd(i+1),vd(i),incr)
      t_fine(j) = rlinear(td(i+1),td(i),incr)
      q_fine(j) = rlinear(qd(i+1),qd(i),incr)   
    end do
  end do

  CALL INTPOL

!  CALL ADVECT
  advgt=advect()


! Calc the initial value of the wind at 50 metres .... Vel, Bri.

  AWIND = SQRT(UD(1)**2+VD(1)**2)

  return
end
