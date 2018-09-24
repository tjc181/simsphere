real pure function splint(XA,YA,Y2A,n,x)
  real, intent(in) :: XA(50), YA(50), Y2A(n)
  integer, intent(in) :: n, x

  integer :: klo, khi, k

  klo=1
  khi=n

  do 
    if (khi-klo .le. 1) exit
    if (khi-klo .gt. 1) then
      k=(khi+klo)/2
      if (XA(k) .gt. x) then
        khi=k
      else
        klo=k
      end if
    end if
  end do

  H=XA(KHI)-XA(KLO)
  A=(XA(KHI)-X)/H
  B=(X-XA(KLO))/H
  splint=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.

end function

!subroutine SPLINT(XA,YA,Y2A,N,X,Y)
!  implicit none
!
!  integer :: n, x, klo, khi, k
!  real :: XA(N),YA(N),Y2A(N)
!  real :: A,B,Y,H
!
!  KLO=1
!  KHI=N
!! Replace this with a do while based on KHI-KLO
!1 IF (KHI-KLO.GT.1) THEN
!    K=(KHI+KLO)/2
!    IF(XA(K).GT.X)THEN
!      KHI=K
!    ELSE
!      KLO=K
!    end if
!    GOTO 1
!  end if
!  H=XA(KHI)-XA(KLO)
!! Pause statement has been deleted from language.  Replace with empty read.
!  if (H.EQ.0.) then
!    write(*,*) 'Bad XA input.'
!    write(*,*) 'Press a key to continue.'
!    read(*,*)
!  end if
!  A=(XA(KHI)-X)/H
!  B=(X-XA(KLO))/H
!  Y=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
!  return
!end
