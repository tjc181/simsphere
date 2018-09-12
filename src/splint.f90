subroutine SPLINT(XA,YA,Y2A,N,X,Y)
  implicit none

  integer :: n, x, klo, khi, k
  real :: XA(N),YA(N),Y2A(N)
  real :: A,B,Y,H

  KLO=1
  KHI=N
! Replace this with a do while based on KHI-KLO
1 IF (KHI-KLO.GT.1) THEN
    K=(KHI+KLO)/2
    IF(XA(K).GT.X)THEN
      KHI=K
    ELSE
      KLO=K
    end if
    GOTO 1
  end if
  H=XA(KHI)-XA(KLO)
! Pause statement has been deleted from language.  Replace with empty read.
  if (H.EQ.0.) then
    write(*,*) 'Bad XA input.'
    write(*,*) 'Press a key to continue.'
    read(*,*)
  end if
  A=(XA(KHI)-X)/H
  B=(X-XA(KLO))/H
  Y=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
  return
end
