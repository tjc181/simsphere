      SUBROUTINE  FINE
  use simsphere_mod

!      INCLUDE 'modvars.h'

          j = 1
          height = 50

          u_fine(1) = ud(1)
          v_fine(1) = vd(1)
          t_fine(1) = td(1)
          q_fine(1) = qd(1)
        
      do 30 i = 1,9          
         do 35 incr = 1,5
          j = j +1  
          u_fine(j) = rlinear(ud(i+1),ud(i),incr) 
          v_fine(j) = rlinear(vd(i+1),vd(i),incr)
          q_fine(j) = rlinear(qd(i+1),qd(i),incr)

	  If (height .le. Hgt) Then
             t_fine(j) = Aptemp
          Else
             t_fine(j) = rlinear(td(i+1),td(i),incr)
	  Endif

          height = height + 50 

 35      continue
 30    continue
      

*/ If below top of the mixing layer potential temp = that at 50 metres
*/ otherwise, average as usual to create fine scale.

       return
       end

       function  rlinear (top,bot,index)

           rlinear = bot + (top - bot) * index /  5

       return
       end
